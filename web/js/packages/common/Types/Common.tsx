
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
}
export interface TableLine {
    id?: string
    connection: string
    action: string
    name: string //connection name
    position: number
    reference: null|Reference
    semantics: string
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
    [index: string]: Connection;
}

export interface TablesMap {
    [index: string]: TableScope
}
