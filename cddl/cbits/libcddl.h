#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

extern "C" {

int8_t *validate_cbor(const uint8_t *cddl_ptr,
                      uintptr_t cddl_len,
                      const uint8_t *cbor_ptr,
                      uintptr_t cbor_len);

} // extern "C"
