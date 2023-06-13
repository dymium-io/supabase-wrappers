package aws

import (
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/comprehend"
)

type ComprehendEntity struct {

	// The zero-based offset from the beginning of the source text to the first
	// character in the entity.
	BeginOffset int

	// The zero-based offset from the beginning of the source text to the last character
	// in the ComprehendEntity.
	EndOffset int

	// The level of confidence that Amazon Comprehend has in the accuracy of the
	// detection.
	Score float64

	// The entity's type.
	Type string
}

const (
	// PiiEntityTypeBankAccountNumber is a PiiEntityType enum value
	PiiEntityTypeBankAccountNumber = "BANK_ACCOUNT_NUMBER"

	// PiiEntityTypeBankRouting is a PiiEntityType enum value
	PiiEntityTypeBankRouting = "BANK_ROUTING"

	// PiiEntityTypeCreditDebitNumber is a PiiEntityType enum value
	PiiEntityTypeCreditDebitNumber = "CREDIT_DEBIT_NUMBER"

	// PiiEntityTypeCreditDebitCvv is a PiiEntityType enum value
	PiiEntityTypeCreditDebitCvv = "CREDIT_DEBIT_CVV"

	// PiiEntityTypeCreditDebitExpiry is a PiiEntityType enum value
	PiiEntityTypeCreditDebitExpiry = "CREDIT_DEBIT_EXPIRY"

	// PiiEntityTypePin is a PiiEntityType enum value
	PiiEntityTypePin = "PIN"

	// PiiEntityTypeEmail is a PiiEntityType enum value
	PiiEntityTypeEmail = "EMAIL"

	// PiiEntityTypeAddress is a PiiEntityType enum value
	PiiEntityTypeAddress = "ADDRESS"

	// PiiEntityTypeName is a PiiEntityType enum value
	PiiEntityTypeName = "NAME"

	// PiiEntityTypePhone is a PiiEntityType enum value
	PiiEntityTypePhone = "PHONE"

	// PiiEntityTypeSsn is a PiiEntityType enum value
	PiiEntityTypeSsn = "SSN"

	// PiiEntityTypeDateTime is a PiiEntityType enum value
	PiiEntityTypeDateTime = "DATE_TIME"

	// PiiEntityTypePassportNumber is a PiiEntityType enum value
	PiiEntityTypePassportNumber = "PASSPORT_NUMBER"

	// PiiEntityTypeDriverId is a PiiEntityType enum value
	PiiEntityTypeDriverId = "DRIVER_ID"

	// PiiEntityTypeUrl is a PiiEntityType enum value
	PiiEntityTypeUrl = "URL"

	// PiiEntityTypeAge is a PiiEntityType enum value
	PiiEntityTypeAge = "AGE"

	// PiiEntityTypeUsername is a PiiEntityType enum value
	PiiEntityTypeUsername = "USERNAME"

	// PiiEntityTypePassword is a PiiEntityType enum value
	PiiEntityTypePassword = "PASSWORD"

	// PiiEntityTypeAwsAccessKey is a PiiEntityType enum value
	PiiEntityTypeAwsAccessKey = "AWS_ACCESS_KEY"

	// PiiEntityTypeAwsSecretKey is a PiiEntityType enum value
	PiiEntityTypeAwsSecretKey = "AWS_SECRET_KEY"

	// PiiEntityTypeIpAddress is a PiiEntityType enum value
	PiiEntityTypeIpAddress = "IP_ADDRESS"

	// PiiEntityTypeMacAddress is a PiiEntityType enum value
	PiiEntityTypeMacAddress = "MAC_ADDRESS"

	// PiiEntityTypeAll is a PiiEntityType enum value
	PiiEntityTypeAll = "ALL"

	// PiiEntityTypeLicensePlate is a PiiEntityType enum value
	PiiEntityTypeLicensePlate = "LICENSE_PLATE"

	// PiiEntityTypeVehicleIdentificationNumber is a PiiEntityType enum value
	PiiEntityTypeVehicleIdentificationNumber = "VEHICLE_IDENTIFICATION_NUMBER"

	// PiiEntityTypeUkNationalInsuranceNumber is a PiiEntityType enum value
	PiiEntityTypeUkNationalInsuranceNumber = "UK_NATIONAL_INSURANCE_NUMBER"

	// PiiEntityTypeCaSocialInsuranceNumber is a PiiEntityType enum value
	PiiEntityTypeCaSocialInsuranceNumber = "CA_SOCIAL_INSURANCE_NUMBER"

	// PiiEntityTypeUsIndividualTaxIdentificationNumber is a PiiEntityType enum value
	PiiEntityTypeUsIndividualTaxIdentificationNumber = "US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER"

	// PiiEntityTypeUkUniqueTaxpayerReferenceNumber is a PiiEntityType enum value
	PiiEntityTypeUkUniqueTaxpayerReferenceNumber = "UK_UNIQUE_TAXPAYER_REFERENCE_NUMBER"

	// PiiEntityTypeInPermanentAccountNumber is a PiiEntityType enum value
	PiiEntityTypeInPermanentAccountNumber = "IN_PERMANENT_ACCOUNT_NUMBER"

	// PiiEntityTypeInNrega is a PiiEntityType enum value
	PiiEntityTypeInNrega = "IN_NREGA"

	// PiiEntityTypeInternationalBankAccountNumber is a PiiEntityType enum value
	PiiEntityTypeInternationalBankAccountNumber = "INTERNATIONAL_BANK_ACCOUNT_NUMBER"

	// PiiEntityTypeSwiftCode is a PiiEntityType enum value
	PiiEntityTypeSwiftCode = "SWIFT_CODE"

	// PiiEntityTypeUkNationalHealthServiceNumber is a PiiEntityType enum value
	PiiEntityTypeUkNationalHealthServiceNumber = "UK_NATIONAL_HEALTH_SERVICE_NUMBER"

	// PiiEntityTypeCaHealthNumber is a PiiEntityType enum value
	PiiEntityTypeCaHealthNumber = "CA_HEALTH_NUMBER"

	// PiiEntityTypeInAadhaar is a PiiEntityType enum value
	PiiEntityTypeInAadhaar = "IN_AADHAAR"

	// PiiEntityTypeInVoterNumber is a PiiEntityType enum value
	PiiEntityTypeInVoterNumber = "IN_VOTER_NUMBER"
)

func Comprehend(txt string) ([]ComprehendEntity, error) {

	sess := session.Must(session.NewSessionWithOptions(session.Options{
		SharedConfigState: session.SharedConfigEnable,
	}))

	// Create a Comprehend client from just a session.
	client := comprehend.New(sess, &aws.Config{Region: aws.String(region)})

	inp := &comprehend.DetectPiiEntitiesInput{}
	inp.SetLanguageCode("en")
	inp.SetText(txt)

	if err := inp.Validate(); err != nil {
		return nil, err
	}
	r, err := client.DetectPiiEntities(inp)
	if err != nil {
		return nil, err
	}

	result := make([]ComprehendEntity, 0, len(r.Entities))
	for _, e := range r.Entities {
		result = append(result, ComprehendEntity{
			BeginOffset: int(*e.BeginOffset),
			EndOffset:   int(*e.EndOffset),
			Score:       *e.Score,
			Type:        *e.Type,
		})
	}
	return result, nil

}
