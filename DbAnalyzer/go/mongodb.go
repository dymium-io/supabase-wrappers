package main

import (
	"DbAnalyzer/detect"
	"DbAnalyzer/types"
	"DbAnalyzer/utils"
	"context"
	"dymium.com/dymium/log"
	"errors"
	"fmt"
	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
	"go.mongodb.org/mongo-driver/mongo"
	"go.mongodb.org/mongo-driver/mongo/options"
	"strings"
	"time"
)

type MongoClient struct {
	mcl *mongo.Client
}

func (cl *MongoClient) Ping() error {
	return cl.mcl.Ping(context.TODO(), nil)
}

func (cl *MongoClient) Close() {
	cl.mcl.Disconnect(context.TODO())
}

func (cl *MongoClient) Connect(c *types.ConnectionParams) error {
	// TODO: add support for different connection types, TLS, etc.
	credential := options.Credential{
		Username: c.User,
		Password: c.Password,
	}
	mdbURI := fmt.Sprintf("mongodb://%s:%d", c.Address, c.Port)
	clientOptions := options.Client().ApplyURI(mdbURI).SetAuth(credential)
	client, err := mongo.Connect(context.TODO(), clientOptions)
	if err != nil {
		log.Errorf("cannot connect to MongoDB, error: [%+v]", err)
		return err
	}
	cl.mcl = client
	if err := cl.Ping(); err != nil {
		cl.mcl = nil
		log.Errorf("cannot ping MongoDB, error: [%+v]", err)
		return err
	}

	return nil
}

func (cl *MongoClient) GetDbInfo(dbName string) (*types.DatabaseInfoData, error) {

	isSystem := false
	switch dbName {
	case "admin", "config", "local":
		isSystem = true
	default:
		isSystem = false
	}

	database := types.DatabaseInfoData{
		DbName:  dbName,
		Schemas: []types.Schema{},
	}
	// MongoDB does not have schemas, so we will use the database name as the schema name
	// and the collection name as the table name
	curSchema := 0
	database.Schemas = append(database.Schemas, types.Schema{
		Name:     dbName,
		IsSystem: isSystem,
		Tables:   []types.Table{},
	})

	db := cl.mcl.Database(dbName)
	collectionNames, err := db.ListCollectionNames(context.TODO(), bson.D{})
	if err != nil {
		log.Errorf("cannot fetch collection list for %s, error: [%+v]", dbName, err)
		return nil, err
	}

	for _, collectionName := range collectionNames {
		isSystemCollection := strings.HasPrefix(collectionName, "system.")
		database.Schemas[curSchema].Tables = append(database.Schemas[curSchema].Tables,
			types.Table{
				Name:     collectionName,
				IsSystem: isSystemCollection,
			})
	}

	return &database, nil
}

type cTypeData struct {
	cName      string
	pos        int
	isNullable bool
	cTyp       string
	eTyp       string
	//cLength, cScale *int
}

func getDocSchema(colmap *map[string]*cTypeData, prefix string, doc bson.M) error {

	for k, v := range doc {
		var d cTypeData
		d.isNullable = true

		//		fmt.Println("key:", k, "value:", v, "type:", fmt.Sprintf("%T", v))
		if k == "_id" {
			// only _id cannot be nullable
			d.isNullable = false
		}

		if prefix != "" {
			k = prefix + "." + k
		}
		switch val := v.(type) {
		case primitive.M:
			// TODO: How deep should we go?
			getDocSchema(colmap, k, v.(bson.M))
			continue
		case primitive.A:
			d.cName = k
			for _, item := range val {
				d.cName = k
				d.cTyp = "ARRAY"
				d.eTyp = fmt.Sprintf("%T", item)
				// We only need to check the first item - in PG arrays are homogeneous
				break
			}
		default:
			d.cName = k
			d.cTyp = fmt.Sprintf("%T", v)
		}
		if (*colmap)[k] == nil {
			(*colmap)[k] = &d
		} else {
			if (*colmap)[k].cTyp == "<nil>" && d.cTyp != "<nil>" {
				(*colmap)[k].cTyp = d.cTyp
			} else if (*colmap)[k].cTyp != "<nil>" && d.cTyp == "<nil>" {
				// do nothing - we already have a type
			} else if (*colmap)[k].cTyp != "<nil>" && d.cTyp == "<nil>" && (*colmap)[k].cTyp != d.cTyp {
				// TODO: how to handle this?
				// for now we keep the first type we found
				log.Warnf("inconsistent type for %s, %s vs %s\n", k, (*colmap)[k].cTyp, d.cTyp)
			}
		}
	}
	return nil
}

func (cl *MongoClient) GetTblInfo(dbName string, tip *types.TableInfoParams) (*types.TableInfoData, error) {
	collection := cl.mcl.Database(tip.Schema).Collection(tip.Table)
	ctx, _ := context.WithTimeout(context.Background(), 100*time.Second)

	// Fetch a sample of documents for schema inference (use a larger sample for more accurate results)
	// TODO: should we use a larger sample or make it configurable?
	cur, err := collection.Find(ctx, bson.D{}, options.Find().SetLimit(detect.SampleSize))
	if err != nil {
		log.Errorf("Error sampling database: [%+v]", err)
		return nil, err
	}
	defer cur.Close(ctx)

	ti := types.TableInfoData{
		DbName:  dbName,
		Schema:  tip.Schema,
		TblName: tip.Table,
	}

	descrMap := make(map[string]*cTypeData)
	for cur.Next(ctx) {
		var doc bson.M
		if err := cur.Decode(&doc); err != nil {
			log.Errorf("cannot fetch collection schema, error: [%+v]", err)
			return nil, err
		}
		err := getDocSchema(&descrMap, "", doc)
		if err != nil {
			log.Errorf("cannot parse collection schema, DB: %s, Collection: %s, error: [%+v]", tip.Schema, tip.Table, err)
			return nil, err
		}
	}
	descr := make([]cTypeData, 0, len(descrMap))
	for _, v := range descrMap {
		log.Infof("%+v\n", *v)
		descr = append(descr, *v)
	}

	detectors, err := detect.Compile(tip.Rules)
	if err != nil {
		return nil, err
	}

	sample := make([]detect.Sample, len(descr))

	obfuscatable := &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Obfuscate, types.DH_Allow}
	allowable := &[]types.DataHandling{types.DH_Block, types.DH_Redact, types.DH_Allow}
	blocked := &[]types.DataHandling{types.DH_Block}

	if len(descr) == 0 {
		log.Errorf("cannot fetch collection schema - collection is empty, DB: %s, Collection: %s", tip.Schema, tip.Table)
		return nil, errors.New("cannot fetch collection schema - collection is empty")
	}

	for k, d := range descr {
		var possibleActions *[]types.DataHandling
		var t string
		var sem *string
		dtk := func(isSamplable bool, sem ...*string) detect.Sample {
			var s *string
			if len(sem) == 1 {
				s = sem[0]
			}
			return detect.Sample{
				IsSamplable: isSamplable,
				IsNullable:  d.isNullable,
				Name:        d.cName,
				Semantics:   s,
			}
		}

		switch strings.ToLower(d.cTyp) {
		case "primitive.objectid":
			t = "name"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "int32":
			t = "integer"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "int64":
			t = "bigint"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "string":
			possibleActions = obfuscatable
			t = "text"
			sample[k] = dtk(true)
		case "float64", "double":
			t = "double precision"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bool":
			t = "boolean"
			possibleActions = allowable
		case "primitive.datetime":
			t = "timestamp"
			possibleActions = allowable
			sample[k] = dtk(true)
		case "bson.m", "primitive.m":
			possibleActions = allowable
			t = "json"
			sample[k] = dtk(true)
		case "array", "primitive.a":
			switch strings.ToLower(d.eTyp) {
			case "int32":
				t = "integer[]"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "int64":
				t = "bigint[]"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "string":
				possibleActions = obfuscatable
				t = "text[]"
				sample[k] = dtk(true)
			case "float64", "double":
				t = "decimal[]"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "bool":
				t = "boolean[]"
				possibleActions = allowable
			case "primitive.datetime":
				t = "timestamp[]"
				possibleActions = allowable
				sample[k] = dtk(true)
			case "bson.m", "primitive.m":
				possibleActions = allowable
				t = "json[]"
				sample[k] = dtk(true)
			default:
				possibleActions = blocked
				t = d.eTyp + "[]"
				sem = utils.Unsupported
				sample[k] = dtk(false, sem)
			}
		default:
			t = d.cTyp
			possibleActions = blocked
			sem = utils.Unsupported
			sample[k] = dtk(false, sem)
		}
		c := types.Column{
			Name:            d.cName,
			Position:        d.pos,
			Typ:             t,
			IsNullable:      d.isNullable,
			Default:         nil,
			Reference:       nil,
			Semantics:       sem,
			PossibleActions: *possibleActions,
		}
		ti.Columns = append(ti.Columns, c)
	}

	if err = cl.getSample(tip.Schema, tip.Table, sample); err != nil {
		return nil, err
	}

	if err = detectors.FindSemantics(sample); err != nil {
		return nil, err
	}

	for k := range ti.Columns {
		if sample[k].Semantics != nil {
			ti.Columns[k].Semantics = sample[k].Semantics
		}
	}
	return &ti, nil
}

func (cl *MongoClient) getSample(schema, table string, sample []detect.Sample) error {

	nColumns := len(sample)

	for _, s := range sample {
		if s.IsSamplable {
			s.Data = make([]*string, 0, detect.SampleSize)
		} else {
			s.Data = make([]*string, 0)
		}
	}

	i := make([]interface{}, 0, nColumns)
	s := make([]string, nColumns)
	p := make([]*string, nColumns)
	var colNames strings.Builder
	start := true
	for k := 0; k != nColumns; k++ {
		if sample[k].IsSamplable {
			if sample[k].IsNullable {
				i = append(i, &p[k])
			} else {
				i = append(i, &s[k])
			}
			if start {
				start = false
			} else {
				colNames.WriteString(", ")
			}
			//colNames.WriteString("[" + sample[k].Name + "]")
			colNames.WriteString("\"" + sample[k].Name + "\"")
		}
	}

	collection := cl.mcl.Database(schema).Collection(table)
	ctx, _ := context.WithTimeout(context.Background(), 100*time.Second)

	// TODO: should we use a larger sample or make it configurable?
	opts := options.Find().SetLimit(detect.SampleSize)
	cur, err := collection.Find(ctx, bson.D{}, opts)
	if err != nil {
		log.Errorf("cannot fetch collection schema, error: [%+v]", err)
		return nil
	}
	defer cur.Close(ctx)

	for cur.Next(ctx) {
		var doc bson.M
		if err := cur.Decode(&doc); err != nil {
			log.Errorf("cannot fetch collection schema, error: [%+v]", err)
			return nil
		}

		for k := range sample {
			if sample[k].IsSamplable {
				var strVal string
				// Convert the value of "fieldName" to string using fmt.Sprintf
				if val, ok := doc[sample[k].Name]; ok {
					strVal = fmt.Sprintf("%v", val)
				} else {
					strVal = ""
				}
				if sample[k].IsNullable {
					if strVal == "" {
						sample[k].Data = append(sample[k].Data, nil)
					} else {
						sample[k].Data = append(sample[k].Data, &strVal)
					}
				} else {
					sample[k].Data = append(sample[k].Data, &strVal)
				}
			}
		}

	}

	return nil
}
