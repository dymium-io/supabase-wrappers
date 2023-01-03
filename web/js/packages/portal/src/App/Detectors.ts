export const defaultDetectors = [
  {
    detector: {
      name: 'Address',
      id: '0000-0000-0000-0001',
      method: 'comprehend',
      data: 'ADDRESS'
    }
  },
  {

    detector: {
      name: 'Age',
      id: '0000-0000-0000-0002',
      method: 'comprehend',
      data: 'AGE'
    }
  },
  {

    detector: {
      name: 'CVV',
      id: '0000-0000-0000-0003',
      method: 'comprehend',
      data: 'CREDIT_DEBIT_CVV'
    }
  },
  {

    detector: {
      name: 'Credit Card Expiry',
      id: '0000-0000-0000-0004',
      method: 'comprehend',
      data: 'CREDIT_DEBIT_EXPIRY'
    }
  },
  {

    detector: {
      name: 'Credit Card Number',
      id: '0000-0000-0000-0005',
      method: 'comprehend',
      data: 'CREDIT_DEBIT_NUMBER'
    }
  },
  {

    detector: {
      name: 'Date/Time',
      id: '0000-0000-0000-0006',
      method: 'comprehend',
      data: 'DATE_TIME'
    }
  },
  {

    detector: {
      name: 'Driver License Number',
      id: '0000-0000-0000-0007',
      method: 'comprehend',
      data: 'DRIVER_ID'
    }
  },
  {

    detector: {
      name: 'Email',
      id: '0000-0000-0000-0008',
      method: 'comprehend',
      data: 'EMAIL'
    }
  },
  {

    detector: {
      name: 'International Bank Account Number',
      id: '0000-0000-0000-0009',
      method: 'comprehend',
      data: 'INTERNATIONAL_BANK_ACCOUNT_NUMBER'
    }
  },
  {

    detector: {
      name: 'IP Address',
      id: '0000-0000-0000-0010',
      method: 'comprehend',
      data: 'IP_ADDRESS'
    }
  },
  {

    detector: {
      name: 'License Plate Number',
      id: '0000-0000-0000-0011',
      method: 'comprehend',
      data: 'LICENSE_PLATE'
    }
  },
  {

    detector: {
      name: 'MAC Address',
      id: '0000-0000-0000-0012',
      method: 'comprehend',
      data: 'MAC_ADDRESS'
    }
  },
  {

    detector: {
      name: 'Name',
      id: '0000-0000-0000-0013',
      method: 'comprehend',
      data: 'NAME'
    }
  },
  {

    detector: {
      name: 'Password',
      id: '0000-0000-0000-0014',
      method: 'comprehend',
      data: 'PASSWORD'
    }
  },
  {

    detector: {
      name: 'Phone',
      id: '0000-0000-0000-0015',
      method: 'comprehend',
      data: 'PHONE'
    }
  },
  {

    detector: {
      name: 'Pin',
      id: '0000-0000-0000-0016',
      method: 'comprehend',
      data: 'PIN'
    }
  },
  {

    detector: {
      name: 'SWIFT Code',
      id: '0000-0000-0000-0017',
      method: 'comprehend',
      data: 'SWIFT_CODE'
    }
  },
  {

    detector: {
      name: 'URL',
      id: '0000-0000-0000-0018',
      method: 'comprehend',
      data: 'URL'
    }
  },
  {

    detector: {
      name: 'Username',
      id: '0000-0000-0000-0019',
      method: 'comprehend',
      data: 'USERNAME'
    }
  },
  {

    detector: {
      name: 'VIN',
      id: '0000-0000-0000-0020',
      method: 'comprehend',
      data: 'VEHICLE_IDENTIFICATION_NUMBER'
    }
  },
  {

    detector: {
      name: 'Bank Account Number',
      id: '0000-0000-0000-0021',
      method: 'comprehend',
      data: 'BANK_ACCOUNT_NUMBER'
    }
  },
  {

    detector: {
      name: 'Bank Routing Number',
      id: '0000-0000-0000-0022',
      method: 'comprehend',
      data: 'BANK_ROUTING'
    }
  },
  {

    detector: {
      name: 'Passport Number',
      id: '0000-0000-0000-0023',
      method: 'comprehend',
      data: 'PASSPORT_NUMBER'
    }
  },
  {

    detector: {
      name: 'Individual TIN',
      id: '0000-0000-0000-0024',
      method: 'comprehend',
      data: 'US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER'
    }
  },
  {
    detector: {
      name: 'SSN',
      id: '0000-0000-0000-0025',
      method: 'comprehend',
      data: 'SSN'
    }
  },
]

export const regexpDetectors = [

  {
    detector: {
      name: 'Processor or device serial number',
      id: '0000-0000-0000-0026',
      method: 'columnregexp',
      data: '(serial|serialnumber|serial_number)'
    },
  },
  {
    detector: {
      name: 'Device ID',
      id: '0000-0000-0000-0027',
      method: 'columnregexp',
      data: '(device_id|deviceid)'
    },
  },
  {
    detector: {
      name: 'Cookie',
      id: '0000-0000-0000-0028',
      method: 'columnregexp',
      data: '(cookie)'
    },
  },
  {
    detector: {
      name: 'Citizenship',
      id: '0000-0000-0000-0029',
      method: 'columnregexp',
      data: '(Citizenship|Nationality)'
    },
  },
  {
    detector: {
      name: 'Visa or immigration status',
      id: '0000-0000-0000-0030',
      method: 'columnregexp',
      data: '(visa|immigration)'
    },
  },
  {
    detector: {
      name: 'Ethnic background',
      id: '0000-0000-0000-0031',
      method: 'columnregexp',
      data: '(ethnic|race|hispanic)'
    },
  },
  {
    detector: {
      name: 'Religious affiliation',
      id: '0000-0000-0000-0032',
      method: 'columnregexp',
      data: '(Religi)'
    },
  },
  {
    detector: {
      name: 'Sexual orientation',
      id: '0000-0000-0000-0033',
      method: 'columnregexp',
      data: '(sexual|gender|male|female)'
    },
  },
  {
    detector: {
      name: 'Criminal history',
      id: '0000-0000-0000-0034',
      method: 'columnregexp',
      data: '(criminal|arrest|felony)'
    },
  },
  {
    detector: {
      name: 'Medical information',
      id: '0000-0000-0000-0035',
      method: 'columnregexp',
      data: '(medical|illness|patient)'
    },
  },
  {
    detector: {
      name: 'Authentication information',
      id: '0000-0000-0000-0036',
      method: 'columnregexp',
      data: '(authentication|login|username)'
    },
  },
  {
    detector: {
      name: 'Country',
      id: '0000-0000-0000-0037',
      method: 'columnregexp',
      data: '(country)'
    },
  },
  {
    detector: {
      name: 'State',
      id: '0000-0000-0000-0038',
      method: 'columnregexp',
      data: '(state)'
    },
  },
  {
    detector: {
      name: 'City',
      id: '0000-0000-0000-0039',
      method: 'columnregexp',
      data: '(city|town|locality)'
    },
  },
  {
    detector: {
      name: 'Zip code',
      id: '0000-0000-0000-0040',
      method: 'columnregexp',
      data: '(Zipcode|zip_code|postalcode|postal_code)'
    },
  },
  {
    detector: {
      name: 'Job position',
      id: '0000-0000-0000-0041',
      method: 'columnregexp',
      data: '(job|position)'
    },
  },
  {
    detector: {
      name: 'Company',
      id: '0000-0000-0000-0043',
      method: 'columnregexp',
      data: '(Company|Work_Place|workplace)'
    },
  },
  {
    detector: {
      name: 'Work Address',
      id: '0000-0000-0000-0044',
      method: 'columnregexp',
      data: '(Work_Address|WorkAddress|work_address)'
    },
  },
  {
    detector: {
      name: 'Vehicle Identification Number',
      id: '0000-0000-0000-0045',
      method: 'columnregexp',
      data: '(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)'
    },
  },
  {
    detector: {
      name: 'Contact Info',
      id: '0000-0000-0000-0046',
      method: 'columnregexp',
      data: '(contact)'
    },
  },

]