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


////// legacy detectors

export const PrefillUnclassified = {
    name: "Unclassified",
    rules: [

        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Allow"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Allow"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Allow"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Allow"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Allow"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Allow"
        },
        {
            regexp: "(Credit card|Credit_card|Creditcard)",
            detection: "Credit card",
            action: "Allow"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Allow"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Allow"
        },
        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Allow"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Allow"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Allow"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Allow"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Allow"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Allow"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Allow"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Allow"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Allow"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Allow"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Allow"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Allow"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Allow"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Allow"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Allow"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Allow"
        },
        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Allow"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Allow"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Allow"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Allow"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Allow"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Allow"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Allow"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Allow"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Allow"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Allow"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Allow"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Allow"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Allow"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Allow"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Allow"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Allow"
        },
        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        },
    ]
}

export const Confidential = {
    name: "Confidential",
    rules: [
        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Obfuscate"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Obfuscate"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Obfuscate"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Obfuscate"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Obfuscate"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Obfuscate"
        },
        {
            regexp: "(Credit card|Credit_card|Creditcard)",
            detection: "Credit card",
            action: "Obfuscate"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Obfuscate"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Obfuscate"
        },

        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Block"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Allow"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Allow"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Obfuscate"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Obfuscate"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Redact"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Redact"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Redact"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Redact"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Redact"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Redact"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Redact"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Smart Redact"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Redact"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Obfuscate"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Obfuscate"
        },
        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Obfuscate"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Obfuscate"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Obfuscate"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Obfuscate"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Obfuscate"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Obfuscate"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Obfuscate"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Obfuscate"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Obfuscate"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Obfuscate"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Obfuscate"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Obfuscate"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Obfuscate"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Obfuscate"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Obfuscate"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Obfuscate"
        },

        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        },
    ]
}
export const Secret = {
    name: "Secret",
    rules: [


        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Redact"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Redact"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Redact"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Redact"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Redact"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Redact"
        },
        {
            regexp: "(Credit card|Credit_card|Creditcard)",
            detection: "Credit card",
            action: "Redact"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Redact"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Redact"
        },

        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Block"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Allow"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Obfuscate"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Obfuscate"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Obfuscate"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Redact"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Redact"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Redact"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Redact"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Redact"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Redact"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Redact"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Smart Redact"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Redact"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Redact"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Obfuscate"
        },

        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Redact"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Redact"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Redact"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Redact"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Redact"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Redact"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Redact"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Redact"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Redact"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Redact"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Redact"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Redact"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Redact"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Redact"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Redact"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Redact"
        },

        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        }
    ]
}
export const TopSecret = {
    name: "Top Secret",
    rules: [
        {
            regexp: "homeaddress",
            detection: "Home address",
            action: "Block"
        },
        {
            regexp: "email",
            detection: "Email address",
            action: "Block"
        },
        {
            regexp: "(SSN|ssn|nationalid|national_id)",
            detection: "Social Security Number",
            action: "Block"
        },
        {
            regexp: "(businessentityid|business_entity_id)",
            detection: "Business entity Id",
            action: "Block"
        },
        {
            regexp: "(passport|Passport number|Passport_number)",
            detection: "Passport number",
            action: "Block"
        },
        {
            regexp: "(Driver license|Driver_license|Driverlicense)",
            detection: "Driver license",
            action: "Block"
        },
        {
            regexp: "(Credit card|Credit_card,Creditcard)",
            detection: "Credit card",
            action: "Block"
        },
        {
            regexp: "(dob|dateofbirth|date_of_birth)",
            detection: "Date of birth",
            action: "Block"
        },
        {
            regexp: "(phone|phonenumber|phone_number)",
            detection: "Telephone number",
            action: "Block"
        },

        {
            regexp: "(login|signin|sign_in|username|user_name)",
            detection: "Login details",
            action: "Block"
        },
        {
            regexp: "(serial|serialnumber|serial_number)",
            detection: "Processor or device serial number",
            action: "Block"
        },
        {
            regexp: "(MAC_address|MACaddress)",
            detection: "MAC address",
            action: "Block"
        },
        {
            regexp: "^(ip|ip_address|ipaddress)$",
            detection: "IP address",
            action: "Block"
        },
        {
            regexp: "(device_id|deviceid)",
            detection: "Device ID",
            action: "Block"
        },
        {
            regexp: "(cookie)",
            detection: "Cookie",
            action: "Block"
        },
        {
            regexp: "(Citizenship|nationality)",
            detection: "Citizenship",
            action: "Block"
        },
        {
            regexp: "(visa|immigration)",
            detection: "Visa or immigration status",
            action: "Block"
        },
        {
            regexp: "(Ethnic)",
            detection: "Ethnic background",
            action: "Block"
        },
        {
            regexp: "(Religi)",
            detection: "Religious affiliation",
            action: "Block"
        },
        {
            regexp: "(sexual)",
            detection: "Sexual orientation",
            action: "Block"
        },
        {
            regexp: "(Criminal)",
            detection: "Criminal history",
            action: "Block"
        },
        {
            regexp: "(Medical)",
            detection: "Medical information",
            action: "Block"
        },
        {
            regexp: "(authentication)",
            detection: "Authentication information",
            action: "Block"
        },
        {
            regexp: "(First name|Firstname|First_name)",
            detection: "First name",
            action: "Block"
        },
        {
            regexp: "(Middle name|Middlename|Middle_name)",
            detection: "Middle name",
            action: "Block"
        },

        {
            regexp: "(last name|lastname|last_name)",
            detection: "Last name",
            action: "Block"
        },
        {
            regexp: "(Country)",
            detection: "Country",
            action: "Block"
        },
        {
            regexp: "(state)",
            detection: "State",
            action: "Block"
        },
        {
            regexp: "(City)",
            detection: "City",
            action: "Block"
        },
        {
            regexp: "(Zipcode|zip_code|zip code|postalcode|postal_code)",
            detection: "Zipcode",
            action: "Block"
        },
        {
            regexp: "(Gender)",
            detection: "Gender",
            action: "Block"
        },
        {
            regexp: "(race)",
            detection: "Race",
            action: "Block"
        },
        {
            regexp: "(age)",
            detection: "Age",
            action: "Block"
        },
        {
            regexp: "(job)",
            detection: "Job position",
            action: "Block"
        },
        {
            regexp: "(Company)",
            detection: "Company",
            action: "Block"
        },
        {
            regexp: "(work_place|workplace)",
            detection: "Work place",
            action: "Block"
        },
        {
            regexp: "(Work_Address|WorkAddress)",
            detection: "Work Address",
            action: "Block"
        },
        {
            regexp: "name",
            detection: "Full Name",
            action: "Block"
        },
        {
            regexp: "(vin|VehicleIdentificationNumber|Vehicle_Identification_Number)",
            detection: "Vehicle Identification Number",
            action: "Block"
        },
        {
            regexp: "(address)",
            detection: "Address",
            action: "Block"
        },
        {
            regexp: "(contact)",
            detection: "Contact Info",
            action: "Block"
        },

        {
            regexp: "",
            detection: "N/A",
            action: "Allow"
        },
    ]
}

