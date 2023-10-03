#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

#define EQN(X, V) printf(X " = %zd\n", (ssize_t)V)
#define CONSTANT(X) EQN(#X, X)

int main() {
  CONSTANT(PROT_NONE);
  CONSTANT(PROT_READ);
  CONSTANT(PROT_WRITE);
  CONSTANT(MAP_ANONYMOUS);
  CONSTANT(MAP_PRIVATE);
  EQN("PAGE_SIZE", sysconf(_SC_PAGESIZE));
  return 0;
}
