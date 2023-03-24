package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"io"
	"os"
	"fmt"
)

func main() {
	switch len(os.Args) {
	case 3:
		if enc, err := AESencrypt([]byte(os.Args[2]), os.Args[1]); err == nil {
			fmt.Printf("%s\n",hex.EncodeToString(enc))
		} else {
			fmt.Printf("%+v\n",err)
		}
	case 4:
		if os.Args[1] != "-d" {
			fmt.Printf("Usage: %s [-d] <key> <text>\n",os.Args[0])
			return
		}
		enc, err := hex.DecodeString(os.Args[3])
		if err != nil {
			fmt.Printf("%+v\n",err)
			return
		}
		if dec, err := AESdecrypt(enc, os.Args[2]); err == nil {
			fmt.Printf("%s\n",string(dec))
		} else {
			fmt.Printf("%+v\n",err)
		}
	default: fmt.Printf("Usage: %s [-d] <key> <text>\n",os.Args[0])
	}
}

func AESencrypt(plaintext []byte, keyhex string) ([]byte, error) {
	key, err := hex.DecodeString(keyhex)
	if err != nil {
		return nil, err
	}

	c, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(c)
	if err != nil {
		return nil, err
	}

	nonce := make([]byte, gcm.NonceSize())
	if _, err = io.ReadFull(rand.Reader, nonce); err != nil {
		return nil, err
	}

	return gcm.Seal(nonce, nonce, plaintext, nil), nil
}

func AESdecrypt(ciphertext []byte, keyhex string) ([]byte, error) {
	key, err := hex.DecodeString(keyhex)
	if err != nil {
		return nil, err
	}

	c, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}

	gcm, err := cipher.NewGCM(c)
	if err != nil {
		return nil, err
	}

	nonceSize := gcm.NonceSize()
	if len(ciphertext) < nonceSize {
		return nil, errors.New("ciphertext too short")
	}

	nonce, ciphertext := ciphertext[:nonceSize], ciphertext[nonceSize:]
	return gcm.Open(nil, nonce, ciphertext, nil)
}
