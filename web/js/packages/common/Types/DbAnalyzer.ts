// this file is automatically generated.
// !!! DO NOT EDIT !!!

import _ from 'lodash'

import * as Common_52182865 from './Common.ts'

export let dirtyFlag = false
function doAlert(s) { console.log(s) }

export type DataType =
  | 'Test'
  | 'DatabaseInfo'
  | 'TableInfo'

export const DataType_as_strings = [ 'Test', 'DatabaseInfo', 'TableInfo' ] as const;



export class AnalyzerRequest {
  private '_dtype': DataType
  private '_connection': ConnectionParams
  private '_tableInfo': TableInfoParams | null

  constructor() {
    this['_dtype'] = 'Test'
    this['_connection'] = new ConnectionParams()
    this['_tableInfo'] = null
  }
  get dtype(): DataType { return this['_dtype'] }
  set dtype(__a__: any) {
    let __v__ = enumReader(['Test','DatabaseInfo','TableInfo'],'Test')(__a__)
    if(!_.isEqual(__v__,this['_dtype'])) {
      setDirtyFlag()
      this['_dtype'] = __v__
    }
  }
  get connection(): ConnectionParams { return this['_connection'] }
  set connection(__a__: any) {
    setDirtyFlag()
    this['_connection'] = __a__
  }
  get tableInfo(): TableInfoParams | null { return this['_tableInfo'] }
  set tableInfo(__a__: any) {
    if(__a__ == null) {
      if(this['_tableInfo'] == null) { return }
      setDirtyFlag()
      this['_tableInfo'] = null
      return
    } else {
      setDirtyFlag()
      this['_tableInfo'] = __a__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): AnalyzerRequest {
    disableDF()
    let cls = new AnalyzerRequest()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.dtype = __a__['dtype']
       cls.connection = ConnectionParams.fromJson(__a__['connection'])
       cls.tableInfo = __a__['tableInfo'] == null ? null : TableInfoParams.fromJson(__a__['tableInfo'])
    } else {
       doAlert(`AnalyzerRequest: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class AnalyzerResponse {
  private '_dtype': DataType
  private '_dbInfo': DatabaseInfoData | null
  private '_tblInfo': TableInfoData | null

  constructor() {
    this['_dtype'] = 'Test'
    this['_dbInfo'] = null
    this['_tblInfo'] = null
  }
  get dtype(): DataType { return this['_dtype'] }
  set dtype(__a__: any) {
    let __v__ = enumReader(['Test','DatabaseInfo','TableInfo'],'Test')(__a__)
    if(!_.isEqual(__v__,this['_dtype'])) {
      setDirtyFlag()
      this['_dtype'] = __v__
    }
  }
  get dbInfo(): DatabaseInfoData | null { return this['_dbInfo'] }
  set dbInfo(__a__: any) {
    if(__a__ == null) {
      if(this['_dbInfo'] == null) { return }
      setDirtyFlag()
      this['_dbInfo'] = null
      return
    } else {
      setDirtyFlag()
      this['_dbInfo'] = __a__
    }
  }
  get tblInfo(): TableInfoData | null { return this['_tblInfo'] }
  set tblInfo(__a__: any) {
    if(__a__ == null) {
      if(this['_tblInfo'] == null) { return }
      setDirtyFlag()
      this['_tblInfo'] = null
      return
    } else {
      setDirtyFlag()
      this['_tblInfo'] = __a__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): AnalyzerResponse {
    disableDF()
    let cls = new AnalyzerResponse()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.dtype = __a__['dtype']
       cls.dbInfo = __a__['dbInfo'] == null ? null : DatabaseInfoData.fromJson(__a__['dbInfo'])
       cls.tblInfo = __a__['tblInfo'] == null ? null : TableInfoData.fromJson(__a__['tblInfo'])
    } else {
       doAlert(`AnalyzerResponse: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class ConnectionDetailRequest {
  private '_connectionId': string

  constructor() {
    this['_connectionId'] = ''
  }
  get connectionId(): string { return this['_connectionId'] }
  set connectionId(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_connectionId'])) {
      setDirtyFlag()
      this['_connectionId'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): ConnectionDetailRequest {
    disableDF()
    let cls = new ConnectionDetailRequest()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.connectionId = __a__['connectionId']
    } else {
       doAlert(`ConnectionDetailRequest: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class ConnectionDetailResponse {
  private '_status': string
  private '_errormessage': string
  private '_response': AnalyzerResponse | null

  constructor() {
    this['_status'] = ''
    this['_errormessage'] = ''
    this['_response'] = null
  }
  get status(): string { return this['_status'] }
  set status(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_status'])) {
      setDirtyFlag()
      this['_status'] = __v__
    }
  }
  get errormessage(): string { return this['_errormessage'] }
  set errormessage(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_errormessage'])) {
      setDirtyFlag()
      this['_errormessage'] = __v__
    }
  }
  get response(): AnalyzerResponse | null { return this['_response'] }
  set response(__a__: any) {
    if(__a__ == null) {
      if(this['_response'] == null) { return }
      setDirtyFlag()
      this['_response'] = null
      return
    } else {
      setDirtyFlag()
      this['_response'] = __a__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): ConnectionDetailResponse {
    disableDF()
    let cls = new ConnectionDetailResponse()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.status = __a__['status']
       cls.errormessage = __a__['errormessage']
       cls.response = __a__['response'] == null ? null : AnalyzerResponse.fromJson(__a__['response'])
    } else {
       doAlert(`ConnectionDetailResponse: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class ConnectionParams {
  private '_typ': Common_52182865.ConnectionType
  private '_address': string
  private '_port': number
  private '_user': string
  private '_password': string
  private '_database': string
  private '_tls': boolean

  constructor() {
    this['_typ'] = 'PostgreSQL'
    this['_address'] = ''
    this['_port'] = 0
    this['_user'] = ''
    this['_password'] = ''
    this['_database'] = ''
    this['_tls'] = false
  }
  get typ(): Common_52182865.ConnectionType { return this['_typ'] }
  set typ(__a__: any) {
    let __v__ = enumReader(['PostgreSQL','MySQL','MariaDB','SqlServer','OracleDB','DB2','MongoDB','Elasticsearch','S3'],'PostgreSQL')(__a__)
    if(!_.isEqual(__v__,this['_typ'])) {
      setDirtyFlag()
      this['_typ'] = __v__
    }
  }
  get address(): string { return this['_address'] }
  set address(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_address'])) {
      setDirtyFlag()
      this['_address'] = __v__
    }
  }
  get port(): number { return this['_port'] }
  set port(__a__: any) {
    let __v__ = intReader(0)(__a__)
    if(!_.isEqual(__v__,this['_port'])) {
      setDirtyFlag()
      this['_port'] = __v__
    }
  }
  get user(): string { return this['_user'] }
  set user(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_user'])) {
      setDirtyFlag()
      this['_user'] = __v__
    }
  }
  get password(): string { return this['_password'] }
  set password(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_password'])) {
      setDirtyFlag()
      this['_password'] = __v__
    }
  }
  get database(): string { return this['_database'] }
  set database(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_database'])) {
      setDirtyFlag()
      this['_database'] = __v__
    }
  }
  get tls(): boolean { return this['_tls'] }
  set tls(__a__: any) {
    let __v__ = boolReader(false)(__a__)
    if(!_.isEqual(__v__,this['_tls'])) {
      setDirtyFlag()
      this['_tls'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): ConnectionParams {
    disableDF()
    let cls = new ConnectionParams()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.typ = __a__['typ']
       cls.address = __a__['address']
       cls.port = __a__['port']
       cls.user = __a__['user']
       cls.password = __a__['password']
       cls.database = __a__['database']
       cls.tls = __a__['tls']
    } else {
       doAlert(`ConnectionParams: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class DatabaseInfoData {
  private '_dbName': string
  private '_schemas': Array<Schema>

  constructor() {
    this['_dbName'] = ''
    this['_schemas'] = []
  }
  get dbName(): string { return this['_dbName'] }
  set dbName(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_dbName'])) {
      setDirtyFlag()
      this['_dbName'] = __v__
    }
  }
  get schemas(): Array<Schema> { return this['_schemas'] }
  set schemas(__a__: any) {
    setDirtyFlag()
    this['_schemas'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): DatabaseInfoData {
    disableDF()
    let cls = new DatabaseInfoData()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.dbName = __a__['dbName']
       cls.schemas = array1Reader(Schema.fromJson)(__a__['schemas'])
    } else {
       doAlert(`DatabaseInfoData: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class TableInfoData {
  private '_dbName': string
  private '_schema': string
  private '_tblName': string
  private '_columns': Array<Column>

  constructor() {
    this['_dbName'] = ''
    this['_schema'] = ''
    this['_tblName'] = ''
    this['_columns'] = []
  }
  get dbName(): string { return this['_dbName'] }
  set dbName(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_dbName'])) {
      setDirtyFlag()
      this['_dbName'] = __v__
    }
  }
  get schema(): string { return this['_schema'] }
  set schema(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_schema'])) {
      setDirtyFlag()
      this['_schema'] = __v__
    }
  }
  get tblName(): string { return this['_tblName'] }
  set tblName(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_tblName'])) {
      setDirtyFlag()
      this['_tblName'] = __v__
    }
  }
  get columns(): Array<Column> { return this['_columns'] }
  set columns(__a__: any) {
    setDirtyFlag()
    this['_columns'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): TableInfoData {
    disableDF()
    let cls = new TableInfoData()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.dbName = __a__['dbName']
       cls.schema = __a__['schema']
       cls.tblName = __a__['tblName']
       cls.columns = array1Reader(Column.fromJson)(__a__['columns'])
    } else {
       doAlert(`TableInfoData: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class TableInfoParams {
  private '_schema': string
  private '_table': string
  private '_rules': Array<Common_52182865.PIIDetector>

  constructor() {
    this['_schema'] = ''
    this['_table'] = ''
    this['_rules'] = []
  }
  get schema(): string { return this['_schema'] }
  set schema(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_schema'])) {
      setDirtyFlag()
      this['_schema'] = __v__
    }
  }
  get table(): string { return this['_table'] }
  set table(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_table'])) {
      setDirtyFlag()
      this['_table'] = __v__
    }
  }
  get rules(): Array<Common_52182865.PIIDetector> { return this['_rules'] }
  set rules(__a__: any) {
    setDirtyFlag()
    this['_rules'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): TableInfoParams {
    disableDF()
    let cls = new TableInfoParams()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.schema = __a__['schema']
       cls.table = __a__['table']
       cls.rules = array1Reader(Common_52182865.PIIDetector.fromJson)(__a__['rules'])
    } else {
       doAlert(`TableInfoParams: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class Column {
  private '_name': string
  private '_position': number
  private '_typ': string
  private '_isNullable': boolean
  private '_default': string | null
  private '_reference': Common_52182865.Reference | null
  private '_semantics': string | null
  private '_possibleActions': Array<Common_52182865.DataHandling>

  constructor() {
    this['_name'] = ''
    this['_position'] = 0
    this['_typ'] = ''
    this['_isNullable'] = false
    this['_default'] = null
    this['_reference'] = null
    this['_semantics'] = null
    this['_possibleActions'] = []
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
    }
  }
  get position(): number { return this['_position'] }
  set position(__a__: any) {
    let __v__ = intReader(0)(__a__)
    if(!_.isEqual(__v__,this['_position'])) {
      setDirtyFlag()
      this['_position'] = __v__
    }
  }
  get typ(): string { return this['_typ'] }
  set typ(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_typ'])) {
      setDirtyFlag()
      this['_typ'] = __v__
    }
  }
  get isNullable(): boolean { return this['_isNullable'] }
  set isNullable(__a__: any) {
    let __v__ = boolReader(false)(__a__)
    if(!_.isEqual(__v__,this['_isNullable'])) {
      setDirtyFlag()
      this['_isNullable'] = __v__
    }
  }
  get default(): string | null { return this['_default'] }
  set default(__a__: any) {
    if(__a__ == null) {
      if(this['_default'] == null) { return }
      setDirtyFlag()
      this['_default'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_default'])) {
        setDirtyFlag()
        this['_default'] = __v__
      }
    }
  }
  get reference(): Common_52182865.Reference | null { return this['_reference'] }
  set reference(__a__: any) {
    if(__a__ == null) {
      if(this['_reference'] == null) { return }
      setDirtyFlag()
      this['_reference'] = null
      return
    } else {
      setDirtyFlag()
      this['_reference'] = __a__
    }
  }
  get semantics(): string | null { return this['_semantics'] }
  set semantics(__a__: any) {
    if(__a__ == null) {
      if(this['_semantics'] == null) { return }
      setDirtyFlag()
      this['_semantics'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_semantics'])) {
        setDirtyFlag()
        this['_semantics'] = __v__
      }
    }
  }
  get possibleActions(): Array<Common_52182865.DataHandling> { return this['_possibleActions'] }
  set possibleActions(__a__: any) {
    setDirtyFlag()
    this['_possibleActions'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): Column {
    disableDF()
    let cls = new Column()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.name = __a__['name']
       cls.position = __a__['position']
       cls.typ = __a__['typ']
       cls.isNullable = __a__['isNullable']
       cls.default = __a__['default'] == null ? null : __a__['default']
       cls.reference = __a__['reference'] == null ? null : Common_52182865.Reference.fromJson(__a__['reference'])
       cls.semantics = __a__['semantics'] == null ? null : __a__['semantics']
       cls.possibleActions = array1Reader(enumReader(['allow','block','obfuscate','redact'],'allow'))(__a__['possibleActions'])
    } else {
       doAlert(`Column: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class Schema {
  private '_name': string
  private '_isSystem': boolean
  private '_tables': Array<Table>

  constructor() {
    this['_name'] = ''
    this['_isSystem'] = false
    this['_tables'] = []
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
    }
  }
  get isSystem(): boolean { return this['_isSystem'] }
  set isSystem(__a__: any) {
    let __v__ = boolReader(false)(__a__)
    if(!_.isEqual(__v__,this['_isSystem'])) {
      setDirtyFlag()
      this['_isSystem'] = __v__
    }
  }
  get tables(): Array<Table> { return this['_tables'] }
  set tables(__a__: any) {
    setDirtyFlag()
    this['_tables'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): Schema {
    disableDF()
    let cls = new Schema()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.name = __a__['name']
       cls.isSystem = __a__['isSystem']
       cls.tables = array1Reader(Table.fromJson)(__a__['tables'])
    } else {
       doAlert(`Schema: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class Table {
  private '_name': string
  private '_isSystem': boolean

  constructor() {
    this['_name'] = ''
    this['_isSystem'] = false
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
    }
  }
  get isSystem(): boolean { return this['_isSystem'] }
  set isSystem(__a__: any) {
    let __v__ = boolReader(false)(__a__)
    if(!_.isEqual(__v__,this['_isSystem'])) {
      setDirtyFlag()
      this['_isSystem'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): Table {
    disableDF()
    let cls = new Table()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.name = __a__['name']
       cls.isSystem = __a__['isSystem']
    } else {
       doAlert(`Table: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

function boolReader(__dflt__) {
  return ((__a__) => {
    if(_.isBoolean(__a__)) {
      return __a__
    }
    doAlert(`boolReader: ${__a__} is not a boolean`)
    if(__a__ === "yes" || __a__ === "true") {
      return true
    }
    if(__a__ === "no" || __a__ === "false") {
      return false
    }
    return __dflt__
  })
}
function intReader(__dflt__) {
  return ((__a__) => {
    if(_.isInteger(__a__)) {
      return __a__
    }
    doAlert(`intReader: ${__a__} is not an integer`)
    if(_.isString(__a__)) {
      let v = parseInt(__a__)
      if(_.isFinite(v)) {
        return v
      }
    }
    return __dflt__
  })
}
function stringReader(__dflt__) {
  return ((__a__) => {
    if(_.isString(__a__)) {
      return __a__
    }
    doAlert(`stringReader: ${__a__} is not a string`)
    return __dflt__
  })
}
function array1Reader(__r__) {
  return ((__a__) => {
    if(!_.isArray(__a__)) {
      doAlert(`arrayReader: ${__a__} is not an array`)
      return []
    }
    return __a__.map(__r__)
  })
}
function enumReader(__v__,__dflt__) {
  return ((__a__) => {
    if(!_.isString(__a__)) {
      doAlert(`enumReader: ${__a__} is not a string`)
      return __dflt__
    }
    if(__a__ !== __dflt__ && !_.includes(__v__,__a__)) {
      doAlert(`enumReader: ${__a__} is not in ${__v__}`)
      return __dflt__
    }
    return __a__
  })
}

function removeLeadingUnderscore(obj: any): any {
  if (Array.isArray(obj)) {
    return obj.map(val => removeLeadingUnderscore(val));
  } else if (typeof obj === 'object' && obj !== null) {
    return Object.keys(obj).reduce((newObj, key) => {
      const newKey = ( key.length > 0 && key[0] === '_' ) ? key.substring(1) : key;
      newObj[newKey] = removeLeadingUnderscore(obj[key]);
      return newObj;
    }, {} as any)
  } else {
    return obj;
  }
}

let setDirtyFlag = () => { dirtyFlag = true }
let [disableDF,enableDF] = (() => {
    let n = 0
    return [() => {
      if(n === 0) {
        setDirtyFlag = () => {}
        n = 1
      } else {
        n += 1
      }
    },
    () => {
      if(n === 1) {
        setDirtyFlag = () => { dirtyFlag = true }
        n = 0
      } else {
        n -= 1
      }
    }]
})()
