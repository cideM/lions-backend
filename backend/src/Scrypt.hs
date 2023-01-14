module Scrypt (verifyPassword, firebaseHashPw) where

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
import Crypto.Cipher.Types (IV, cipherInit, ctrCombine, nullIV)
import Crypto.Error (CryptoFailable (..))
import Crypto.KDF.Scrypt (Parameters (..), generate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64
import Data.Text
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

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
           userSaltDec <- mapLeft (\err -> "error decoding user salt: " <> err) $ decodeBase64 userSalt
           signerKeyDec <- mapLeft (\err -> "error decoding signer key: " <> err) $ decodeBase64 signerKey
           saltSepDec <- mapLeft (\err -> "error decoding salt separator: " <> err) $ decodeBase64 saltSep
           return (userSaltDec, signerKeyDec, saltSepDec)
       ) of
    Left e -> Left e
    Right (userSaltDec, signerKeyDec, saltSepDec) ->
      let params =
            Parameters
              { n = (fromIntegral $ (2 :: Integer) ^ memcost), -- \^ Cpu/Memory cost ratio. must be a power of 2 greater than 1. also known as N.
                r = (fromIntegral rounds), -- \^ Must satisfy r * p < 2^30
                p = 1, -- \^ Must satisfy r * p < 2^30
                outputLength = 32 -- \^ the number of bytes to generate out of Scrypt
              }
          (derivedKey :: ByteString) = generate params pw (userSaltDec <> saltSepDec)
       in case cipherInit derivedKey of
            CryptoFailed e -> Left $ "error in cipherInit: " <> (Text.pack $ show e)
            CryptoPassed context ->
              Right . encodeBase64 $ ctrCombine context (nullIV :: IV AES256) signerKeyDec

verifyPassword ::
  BS.ByteString -> -- Project's base64_signer_key
  BS.ByteString -> -- Project's base64_salt_separator
  BS.ByteString -> -- User's salt
  BS.ByteString -> -- Existing password from database
  BS.ByteString -> -- Input that we want to verify
  Either Text Bool
verifyPassword signerKey saltSep userSalt want have = do
  hashed <- encodeUtf8 <$> firebaseHashPw userSalt signerKey saltSep 14 8 have
  return $ want == hashed
