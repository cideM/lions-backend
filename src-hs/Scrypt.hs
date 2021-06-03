module Scrypt where

-- https://github.com/firebase/scrypt/issues/2
-- https://github.com/JaakkoL/firebase-scrypt-python/blob/master/firebasescrypt/firebasescrypt.py
-- Sorry, Firebase doesn't use the standard Scrypt Algorithm. We use just the derived key (taken from the password, salt, and salt separator), and then do an extra round of AES using it and the project-level signing key.
-- You can take a look at the function scryptenc_buf_saltlen, and where it's called
-- We also do a layer of base64 encoding/decoding
-- The outline of it is:
--     Decrypt the User's salt, and Project's base64_signer_key and base64_salt_separator from base64
--     Run crypto.scrypt function with the parameters:
--     password = User's password
--     salt = User's salt + salt_separator
--     options.N = 2 ^ mem_cost
--     options.r = rounds
--     options.p = 1
--     Then take the returned derived Key, and run AES on it, with the key being the derived key, and the input being the project's signer_key (decrypted from base64)
--     Encode the result using base64

import Crypto.Cipher.AES
import Crypto.Cipher.Types (IV, nullIV)
import Crypto.Scrypt
import qualified Data.ByteString as BS
import Data.ByteString.Base64
import Data.Text

firebaseHashPw ::
  BS.ByteString -> -- User's salt
  BS.ByteString -> -- Project's base64_signer_key
  BS.ByteString -> -- Project's base64_salt_separator
  Integer -> -- Mem cost
  Integer -> -- rounds
  BS.ByteString -> -- Password
  Either Text Text
firebaseHashPw userSalt signerKey saltSep memcost rounds pw =
  case ( do
           userSaltDec <- decodeBase64 userSalt
           signerKeyDec <- decodeBase64 signerKey
           saltSepDec <- decodeBase64 saltSep
           return (userSaltDec, signerKeyDec, saltSepDec)
       ) of
    Left e -> Left e
    Right (userSaltDec, signerKeyDec, saltSepDec) ->
      case scryptParams memcost rounds 1 of
        Nothing -> Left "couldn't create scrypt params"
        Just params ->
          let derivedKey = getHash $ scrypt params (Salt (userSaltDec <> saltSepDec)) (Pass pw)
              aes = initAES (BS.take 32 derivedKey)
           in Right . encodeBase64 $ encryptCTR aes (nullIV :: IV AES) signerKeyDec
