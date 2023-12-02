// this file is automatically generated.
// !!! DO NOT EDIT !!!

import _ from 'lodash'



export let dirtyFlag = false
function doAlert(s) { console.log(s) }

export type ConnectorStatus =
  | 'provisioned'
  | 'configured'

export const ConnectorStatus_as_strings = [ 'provisioned', 'configured' ] as const;

export function humanReadableConnectorStatus(__a__ : ConnectorStatus) : string {
  switch(__a__) {
    case 'provisioned': return 'Provisioned';
    case 'configured': return 'Used in Data Source';
  }
  return '';
}

export type TunnelStatus =
  | 'provisioned'
  | 'configured'
  | 'active'

export const TunnelStatus_as_strings = [ 'provisioned', 'configured', 'active' ] as const;

export function humanReadableTunnelStatus(__a__ : TunnelStatus) : string {
  switch(__a__) {
    case 'provisioned': return 'Provisioned';
    case 'configured': return 'Used in Data Source';
    case 'active': return 'Tunnel Up';
  }
  return '';
}

export class AddConnectorRequest {
  private '_id': string | null
  private '_name': string
  private '_accesskey': string
  private '_secret': string
  private '_tunnels': Array<Tunnel>

  constructor() {
    this['_id'] = null
    this['_name'] = ''
    this['_accesskey'] = ''
    this['_secret'] = ''
    this['_tunnels'] = []
  }
  get id(): string | null { return this['_id'] }
  set id(__a__: any) {
    if(__a__ == null) {
      if(this['_id'] == null) { return }
      setDirtyFlag()
      this['_id'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_id'])) {
        setDirtyFlag()
        this['_id'] = __v__
      }
    }
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
    }
  }
  get accesskey(): string { return this['_accesskey'] }
  set accesskey(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_accesskey'])) {
      setDirtyFlag()
      this['_accesskey'] = __v__
    }
  }
  get secret(): string { return this['_secret'] }
  set secret(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_secret'])) {
      setDirtyFlag()
      this['_secret'] = __v__
    }
  }
  get tunnels(): Array<Tunnel> { return this['_tunnels'] }
  set tunnels(__a__: any) {
    setDirtyFlag()
    this['_tunnels'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): AddConnectorRequest {
    disableDF()
    let cls = new AddConnectorRequest()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.id = __a__['id'] == null ? null : __a__['id']
       cls.name = __a__['name']
       cls.accesskey = __a__['accesskey']
       cls.secret = __a__['secret']
       cls.tunnels = array1Reader(Tunnel.fromJson)(__a__['tunnels'])
    } else {
       doAlert(`AddConnectorRequest: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class AuthorizationCodeRequest {
  private '_customerid': string
  private '_code': string

  constructor() {
    this['_customerid'] = ''
    this['_code'] = ''
  }
  get customerid(): string { return this['_customerid'] }
  set customerid(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_customerid'])) {
      setDirtyFlag()
      this['_customerid'] = __v__
    }
  }
  get code(): string { return this['_code'] }
  set code(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_code'])) {
      setDirtyFlag()
      this['_code'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): AuthorizationCodeRequest {
    disableDF()
    let cls = new AuthorizationCodeRequest()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.customerid = __a__['customerid']
       cls.code = __a__['code']
    } else {
       doAlert(`AuthorizationCodeRequest: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class AuthorizationCodeResponse {
  private '_token': string
  private '_name': string
  private '_groups': Array<string>

  constructor() {
    this['_token'] = ''
    this['_name'] = ''
    this['_groups'] = []
  }
  get token(): string { return this['_token'] }
  set token(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_token'])) {
      setDirtyFlag()
      this['_token'] = __v__
    }
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
    }
  }
  get groups(): Array<string> { return this['_groups'] }
  set groups(__a__: any) {
    setDirtyFlag()
    this['_groups'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): AuthorizationCodeResponse {
    disableDF()
    let cls = new AuthorizationCodeResponse()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.token = __a__['token']
       cls.name = __a__['name']
       cls.groups = array1Reader(stringReader(''))(__a__['groups'])
    } else {
       doAlert(`AuthorizationCodeResponse: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class CSRResponse {
  private '_version': string
  private '_certificate': string

  constructor() {
    this['_version'] = ''
    this['_certificate'] = ''
  }
  get version(): string { return this['_version'] }
  set version(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_version'])) {
      setDirtyFlag()
      this['_version'] = __v__
    }
  }
  get certificate(): string { return this['_certificate'] }
  set certificate(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_certificate'])) {
      setDirtyFlag()
      this['_certificate'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): CSRResponse {
    disableDF()
    let cls = new CSRResponse()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.version = __a__['version']
       cls.certificate = __a__['certificate']
    } else {
       doAlert(`CSRResponse: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class CertificateRequest {
  private '_csr': string

  constructor() {
    this['_csr'] = ''
  }
  get csr(): string { return this['_csr'] }
  set csr(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_csr'])) {
      setDirtyFlag()
      this['_csr'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): CertificateRequest {
    disableDF()
    let cls = new CertificateRequest()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.csr = __a__['csr']
    } else {
       doAlert(`CertificateRequest: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class CertificateRequestWithSecret {
  private '_customer': string
  private '_key': string
  private '_secret': string
  private '_csr': string

  constructor() {
    this['_customer'] = ''
    this['_key'] = ''
    this['_secret'] = ''
    this['_csr'] = ''
  }
  get customer(): string { return this['_customer'] }
  set customer(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_customer'])) {
      setDirtyFlag()
      this['_customer'] = __v__
    }
  }
  get key(): string { return this['_key'] }
  set key(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_key'])) {
      setDirtyFlag()
      this['_key'] = __v__
    }
  }
  get secret(): string { return this['_secret'] }
  set secret(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_secret'])) {
      setDirtyFlag()
      this['_secret'] = __v__
    }
  }
  get csr(): string { return this['_csr'] }
  set csr(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_csr'])) {
      setDirtyFlag()
      this['_csr'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): CertificateRequestWithSecret {
    disableDF()
    let cls = new CertificateRequestWithSecret()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.customer = __a__['customer']
       cls.key = __a__['key']
       cls.secret = __a__['secret']
       cls.csr = __a__['csr']
    } else {
       doAlert(`CertificateRequestWithSecret: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class Connector {
  private '_id': string
  private '_name': string
  private '_accesskey': string | null
  private '_secret': string | null
  private '_status': TunnelStatus | null
  private '_tunnels': Array<Tunnel>

  constructor() {
    this['_id'] = ''
    this['_name'] = ''
    this['_accesskey'] = null
    this['_secret'] = null
    this['_status'] = null
    this['_tunnels'] = []
  }
  get id(): string { return this['_id'] }
  set id(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_id'])) {
      setDirtyFlag()
      this['_id'] = __v__
    }
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
    }
  }
  get accesskey(): string | null { return this['_accesskey'] }
  set accesskey(__a__: any) {
    if(__a__ == null) {
      if(this['_accesskey'] == null) { return }
      setDirtyFlag()
      this['_accesskey'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_accesskey'])) {
        setDirtyFlag()
        this['_accesskey'] = __v__
      }
    }
  }
  get secret(): string | null { return this['_secret'] }
  set secret(__a__: any) {
    if(__a__ == null) {
      if(this['_secret'] == null) { return }
      setDirtyFlag()
      this['_secret'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_secret'])) {
        setDirtyFlag()
        this['_secret'] = __v__
      }
    }
  }
  get status(): TunnelStatus | null { return this['_status'] }
  set status(__a__: any) {
    if(__a__ == null) {
      if(this['_status'] == null) { return }
      setDirtyFlag()
      this['_status'] = null
      return
    } else {
      let __v__ = enumReader(['provisioned','configured','active'],'provisioned')(__a__)
      if(!_.isEqual(__v__,this['_status'])) {
        setDirtyFlag()
        this['_status'] = __v__
      }
    }
  }
  get tunnels(): Array<Tunnel> { return this['_tunnels'] }
  set tunnels(__a__: any) {
    setDirtyFlag()
    this['_tunnels'] = __a__
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): Connector {
    disableDF()
    let cls = new Connector()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.id = __a__['id']
       cls.name = __a__['name']
       cls.accesskey = __a__['accesskey'] == null ? null : __a__['accesskey']
       cls.secret = __a__['secret'] == null ? null : __a__['secret']
       cls.status = __a__['status'] == null ? null : __a__['status']
       cls.tunnels = array1Reader(Tunnel.fromJson)(__a__['tunnels'])
    } else {
       doAlert(`Connector: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class CustomerIDRequest {
  private '_customerid': string

  constructor() {
    this['_customerid'] = ''
  }
  get customerid(): string { return this['_customerid'] }
  set customerid(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_customerid'])) {
      setDirtyFlag()
      this['_customerid'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): CustomerIDRequest {
    disableDF()
    let cls = new CustomerIDRequest()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.customerid = __a__['customerid']
    } else {
       doAlert(`CustomerIDRequest: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class CustomerIDResponse {
  private '_loginURL': string
  private '_lbaddress': string
  private '_lbport': number
  private '_protocolVersion': string
  private '_clientMajorVersion': string
  private '_clientMinorVersion': string

  constructor() {
    this['_loginURL'] = ''
    this['_lbaddress'] = ''
    this['_lbport'] = 0
    this['_protocolVersion'] = ''
    this['_clientMajorVersion'] = ''
    this['_clientMinorVersion'] = ''
  }
  get loginURL(): string { return this['_loginURL'] }
  set loginURL(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_loginURL'])) {
      setDirtyFlag()
      this['_loginURL'] = __v__
    }
  }
  get lbaddress(): string { return this['_lbaddress'] }
  set lbaddress(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_lbaddress'])) {
      setDirtyFlag()
      this['_lbaddress'] = __v__
    }
  }
  get lbport(): number { return this['_lbport'] }
  set lbport(__a__: any) {
    let __v__ = intReader(0)(__a__)
    if(!_.isEqual(__v__,this['_lbport'])) {
      setDirtyFlag()
      this['_lbport'] = __v__
    }
  }
  get protocolVersion(): string { return this['_protocolVersion'] }
  set protocolVersion(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_protocolVersion'])) {
      setDirtyFlag()
      this['_protocolVersion'] = __v__
    }
  }
  get clientMajorVersion(): string { return this['_clientMajorVersion'] }
  set clientMajorVersion(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_clientMajorVersion'])) {
      setDirtyFlag()
      this['_clientMajorVersion'] = __v__
    }
  }
  get clientMinorVersion(): string { return this['_clientMinorVersion'] }
  set clientMinorVersion(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_clientMinorVersion'])) {
      setDirtyFlag()
      this['_clientMinorVersion'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): CustomerIDResponse {
    disableDF()
    let cls = new CustomerIDResponse()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.loginURL = __a__['loginURL']
       cls.lbaddress = __a__['lbaddress']
       cls.lbport = __a__['lbport']
       cls.protocolVersion = __a__['protocolVersion']
       cls.clientMajorVersion = __a__['clientMajorVersion']
       cls.clientMinorVersion = __a__['clientMinorVersion']
    } else {
       doAlert(`CustomerIDResponse: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class GetKeySecret {
  private '_accesskey': string
  private '_secret': string

  constructor() {
    this['_accesskey'] = ''
    this['_secret'] = ''
  }
  get accesskey(): string { return this['_accesskey'] }
  set accesskey(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_accesskey'])) {
      setDirtyFlag()
      this['_accesskey'] = __v__
    }
  }
  get secret(): string { return this['_secret'] }
  set secret(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_secret'])) {
      setDirtyFlag()
      this['_secret'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): GetKeySecret {
    disableDF()
    let cls = new GetKeySecret()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.accesskey = __a__['accesskey']
       cls.secret = __a__['secret']
    } else {
       doAlert(`GetKeySecret: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class MachineCSRResponse {
  private '_version': string
  private '_jwt': string
  private '_certificate': string

  constructor() {
    this['_version'] = ''
    this['_jwt'] = ''
    this['_certificate'] = ''
  }
  get version(): string { return this['_version'] }
  set version(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_version'])) {
      setDirtyFlag()
      this['_version'] = __v__
    }
  }
  get jwt(): string { return this['_jwt'] }
  set jwt(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_jwt'])) {
      setDirtyFlag()
      this['_jwt'] = __v__
    }
  }
  get certificate(): string { return this['_certificate'] }
  set certificate(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_certificate'])) {
      setDirtyFlag()
      this['_certificate'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): MachineCSRResponse {
    disableDF()
    let cls = new MachineCSRResponse()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.version = __a__['version']
       cls.jwt = __a__['jwt']
       cls.certificate = __a__['certificate']
    } else {
       doAlert(`MachineCSRResponse: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class RequestUpdate {
  private '_protocolVersion': string
  private '_clientMajorVersion': string
  private '_clientMinorVersion': string

  constructor() {
    this['_protocolVersion'] = ''
    this['_clientMajorVersion'] = ''
    this['_clientMinorVersion'] = ''
  }
  get protocolVersion(): string { return this['_protocolVersion'] }
  set protocolVersion(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_protocolVersion'])) {
      setDirtyFlag()
      this['_protocolVersion'] = __v__
    }
  }
  get clientMajorVersion(): string { return this['_clientMajorVersion'] }
  set clientMajorVersion(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_clientMajorVersion'])) {
      setDirtyFlag()
      this['_clientMajorVersion'] = __v__
    }
  }
  get clientMinorVersion(): string { return this['_clientMinorVersion'] }
  set clientMinorVersion(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_clientMinorVersion'])) {
      setDirtyFlag()
      this['_clientMinorVersion'] = __v__
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): RequestUpdate {
    disableDF()
    let cls = new RequestUpdate()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.protocolVersion = __a__['protocolVersion']
       cls.clientMajorVersion = __a__['clientMajorVersion']
       cls.clientMinorVersion = __a__['clientMinorVersion']
    } else {
       doAlert(`RequestUpdate: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
}

export class Tunnel {
  private '_id': string | null
  private '_name': string
  private '_address': string
  private '_port': string
  private '_localport': string | null
  private '_status': ConnectorStatus | null

  constructor() {
    this['_id'] = null
    this['_name'] = ''
    this['_address'] = ''
    this['_port'] = ''
    this['_localport'] = null
    this['_status'] = null
  }
  get id(): string | null { return this['_id'] }
  set id(__a__: any) {
    if(__a__ == null) {
      if(this['_id'] == null) { return }
      setDirtyFlag()
      this['_id'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_id'])) {
        setDirtyFlag()
        this['_id'] = __v__
      }
    }
  }
  get name(): string { return this['_name'] }
  set name(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_name'])) {
      setDirtyFlag()
      this['_name'] = __v__
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
  get port(): string { return this['_port'] }
  set port(__a__: any) {
    let __v__ = stringReader('')(__a__)
    if(!_.isEqual(__v__,this['_port'])) {
      setDirtyFlag()
      this['_port'] = __v__
    }
  }
  get localport(): string | null { return this['_localport'] }
  set localport(__a__: any) {
    if(__a__ == null) {
      if(this['_localport'] == null) { return }
      setDirtyFlag()
      this['_localport'] = null
      return
    } else {
      let __v__ = stringReader('')(__a__)
      if(!_.isEqual(__v__,this['_localport'])) {
        setDirtyFlag()
        this['_localport'] = __v__
      }
    }
  }
  get status(): ConnectorStatus | null { return this['_status'] }
  set status(__a__: any) {
    if(__a__ == null) {
      if(this['_status'] == null) { return }
      setDirtyFlag()
      this['_status'] = null
      return
    } else {
      let __v__ = enumReader(['provisioned','configured'],'provisioned')(__a__)
      if(!_.isEqual(__v__,this['_status'])) {
        setDirtyFlag()
        this['_status'] = __v__
      }
    }
  }

  toJson(): string { return JSON.stringify(removeLeadingUnderscore(this)); }

  static fromJson(__a__: any): Tunnel {
    disableDF()
    let cls = new Tunnel()
    if(typeof __a__ === 'object' && __a__ != null) {
       cls.id = __a__['id'] == null ? null : __a__['id']
       cls.name = __a__['name']
       cls.address = __a__['address']
       cls.port = __a__['port']
       cls.localport = __a__['localport'] == null ? null : __a__['localport']
       cls.status = __a__['status'] == null ? null : __a__['status']
    } else {
       doAlert(`Tunnel: an attempt to initialize from ${__a__}`)
    }
    enableDF()
    return cls
  }
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
