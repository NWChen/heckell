CC = gcc
CGLAGS = -g -Wall -Werror
LDFLAGS = -g

ut-sets: ut-sets.o

ut-sets.o: uthash.h

PHONY += clean
clean:
	rm -rf $(TARGETS) a.out *.o
