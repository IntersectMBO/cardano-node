{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.SSL.Certs
  ( placeDefaultSSLFiles
  ) where

import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.RTView.System

import           Control.Exception.Extra (ignore)
import           Control.Monad.Extra (unlessM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.String.QQ
import qualified System.Directory as D

placeDefaultSSLFiles :: TracerEnv -> IO (FilePath, FilePath)
placeDefaultSSLFiles tracerEnv = do
  (pathToCertFile, pathToKeyFile) <- getPathsToSSLCerts tracerEnv
  writeIfNeeded pathToCertFile defaultCert
  writeIfNeeded pathToKeyFile  defaultKey
  -- Set permissions like 'openssl' does.
  D.setPermissions pathToCertFile (D.setOwnerWritable True $ D.emptyPermissions { D.readable = True })     -- 0644
  D.setPermissions pathToKeyFile  (D.setOwnerWritable True . D.setOwnerReadable True $ D.emptyPermissions) -- 0600
  return (pathToCertFile, pathToKeyFile)
 where
  writeIfNeeded p f =
    unlessM (D.doesFileExist p) . ignore $
      BS.writeFile p f

-- By default we use self-signed 'cert.pem' certificate and 'key.pem' key created
-- by 'openssl' program. Example of the command:
--
-- $ openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -days 365 -nodes

defaultCert :: ByteString
defaultCert = [s|
-----BEGIN CERTIFICATE-----
MIIFazCCA1OgAwIBAgIUNIm8PPV3chKFhFj/A/nd4cdHWJIwDQYJKoZIhvcNAQEL
BQAwRTELMAkGA1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUxITAfBgNVBAoM
GEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDAeFw0yMjA1MjQwODA5MTRaFw0yMzA1
MjQwODA5MTRaMEUxCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEw
HwYDVQQKDBhJbnRlcm5ldCBXaWRnaXRzIFB0eSBMdGQwggIiMA0GCSqGSIb3DQEB
AQUAA4ICDwAwggIKAoICAQC59W9J7xYp+6RaTla/wIZja5A7mpl+3KSHTvSOds34
jGigw7aAM8VzUGG7Gkx3oFq5LmB1i4GwTsaUHtPLgvbkkwzz83B0ZIr9Ox5uhVa+
m7pYpAgKhMjLRc11KF2cIfM8NOqDwmwM/+c1SsDegSVFViJrEwFUfQTeYjG7mbLB
wia4pzEj6Ou3URRFCoCxBon/4/321vieDVsK+eLEUHFROyLXDkIQGcDhbsbQLt0r
GZ2chlpSJvzJorn0/tWM8VNZCfKu6sab58HT+TgJDTZ9lwIbeMNWPB3zHTGurxkB
w8Po48tlz41kiArKiWF9vgB9ObY3AY8l2CWKASPhGwYW1Ndkeqn+78oB29HY92oJ
LozBA5kkDQgbo51lQlobGRVsS6cotu3iqIUHigjbXH1+/Nch2eRPxg/5OcdBpPix
qQJOP7GM+krNt3iDA9DkFGKWTf11p0eZRODWE+ACJU5tQY9QaCQ4d2HKGZm4gWdz
Vedvck2GhdnZi9RzL0UVRUzkTpEf6nIChn8lZP0+48c69E6DHV9xXB4mNTkuHYEO
iQ8EVQDXIxjRVUjDaccxs6snAewWOygySweXjc0XFEdogMfzldpeS669HWcmMbbt
dkEUy6zC11dKVaHn1Ou9sjz1mnzHXAjrDEcsPy7SFabqLxRnrhRRs948RFvjQzYp
QwIDAQABo1MwUTAdBgNVHQ4EFgQUsAwUYTw3B5PyadhB/nYduV8HDgEwHwYDVR0j
BBgwFoAUsAwUYTw3B5PyadhB/nYduV8HDgEwDwYDVR0TAQH/BAUwAwEB/zANBgkq
hkiG9w0BAQsFAAOCAgEAgakOs52QWC/jGDbLxMD55Xj3WtG0jwZM+1RzFeIemEMK
3pspcyNUVM+CoAMrCilH5es+giFvbo3KDS/wYLMkAYJNdSSoj3OQq7PBrkt2lkCi
NBF3Ul4Mip1jMVwgzyFNqNCQaz/y/Hdm+4GQ1vJlLEKhOL3h8b5icK0+jgY81PiC
th5wlE26JyVD+smYg25VMqcTYyFNSZ/I4bPE2isxWw3EXOrBtP8MwqsPw1swWyD2
9pQx2evU+XDNvQ3cN/q9lZckbH5qytxNCGds5PO249bE1WRZLznLj/VDKFSTyYul
CR7Qcg6GguwxMmIYCfkHaF+ivOEtJGs5JKaCXxjBsOvxW6exxDe1aTmU4C+GpTeU
1HPowrfp5HzRP9BbDV37//w1dFcLp3cxKpOjFpVfpv9YUH6FvQid57OeC7kIq90N
Zi4jxHH7DV5DozLARlp61CKSuV6LbBNqDywtwoZPK9TNCqvdXa504hFw4KpdRWPi
A8Lr1iudUNDxwq+bbSrxPUKL1i/6h/yU5nz469iMUzgKtJkhpbT6kaz9zBgClxeG
GZHU7nKd8Ay85+b59RM7hrkc9miSgyY/uLaUHN1klYllbtBJvpR1MPQbcNSny09x
gx0QcX6nEDv/K8XuOvzndTRCuOB9R6ly0i+hfrTXi2FnlOnZ93U6FfrRXagsTrk=
-----END CERTIFICATE-----
|]

defaultKey :: ByteString
defaultKey = [s|
-----BEGIN PRIVATE KEY-----
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
+Jz2VtvTX0rLiK/CnJ59JvIpniHtAIgf
-----END PRIVATE KEY-----
|]

