import * as types from '@dymium/common/Types/Common'
export interface Reference {
    schema: string
    table: string
    column: string
}
export interface DatascopeRecord {
    connection: string
    schema: string
    table: string
    typ: string
    position: number
    reference: null|Reference
    action: string
    col: string
    semantics: string    
    dflt: string
    isnullable: boolean
    possibleActions: string[]
}
export interface TableLine {
    id?: string
    connection: string
    action: string
    name: string //connection name
    position: number
    reference: null|Reference
    semantics: string
    possibleActions?: string[]
    typ: string
    dflt: string
    isnullable: boolean    
  }
  export interface TableScope {
    connection?: string
    schema: string
    table: string
    tablescope?: Array<TableLine>
}
export interface Connection {
    id?: string
    credid?:string
    dbtype: string
    name: string

    address: string
    port: number
    description: string
    usetls: boolean
}
export interface DataScope { 
    name: string
    records: Array<DatascopeRecord>

}
export class ConnectionMap {
    [index: string]: types.ConnectionRecord;
}

export interface TablesMap {
    [index: string]: TableScope
}

export interface Group {
    id: string;
    name: string;
}
export interface DataScopeInfo {
    id: string;
    name: string;
    records?: TableLine[];
    groups?: Group[];
}

export interface Mapping {
    id?: string;
    dymiumgroup: string;
    directorygroup: string;
    comments: string;
    adminaccess: boolean;
}