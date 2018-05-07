#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include "uthash.h"

// 
#define BUFF_MULT 1.5

/*
 * [NOTE RE: ut-hash usage]
 * In order to change the set itself in the ut-hash library,
 * one would normally have to pass in struct hset_head**
 * to change the set beyond scope of that function.
 * However, since Heckell never modifies the set itself,
 * instead copying the table at the pointer, we can pass in
 * struct hset_head* and return struct hset_head*.
 */

enum types {
	INT, REAL, CHAR, BOOL,
	STRING, SET
};

// max num of char in string of type
// -1 means should not be used
// includes 2 char padding
int type_str_size[] = {
	14, 100, 6, 8, -1, -1
};

char *types[] = {
	"int", "real", "char", "bool", "string", "set"
};

struct hset_head {
	char *val_ts;
	void *val_p;
	//char *typ;
	UT_hash_handle hh;
};

int mystrcmp(const char *str1, const char *str2) {
	return strcasecmp(str1, str2) == 0;
}

/* Return number of elements in tuple, divide typ with null
 * characters
 */
int parse_type(char *typ) {
	int n = 1;
	int p = 0;
	int len = strlen(typ);
	for (int i = 0; i < len; i++) {
		char c = typ[i];
		if (c == ')') {
			p--;
			if (p == 0) typ[i] = ' ';
			continue;
		} else if (c == '(') {
			if (p == 0) typ[i] = ' ';
			p++;
			continue;
		}
		if (p > 0) {
			continue;
		}

		if (c == '*') {
			n++;
			typ[i] = '\0';
			typ[i-1] = '\0';
			typ[i+1] = '\0';
		}
	}

	return n;
}

/* Copies str into larger buffer and frees old str */
char *resize_string(char *str, size_t n) {
	char *new_str = malloc(n);
	strcpy(new_str, str);
	free(str);
	return new_str;
}

/* Converts val to string. Return value needs 
 * to be freed by caller
 */
char *string_of(void *val, char *typ) {
	// copy typ so we can modify it
	char *ctyp = strdup(typ);
	char *og_ctyp = ctyp;
	int t_l = strlen(ctyp);
	// trim parentheses
	if (ctyp[t_l-1] == ')') {
		ctyp[t_l-1] = '\0';
		ctyp++;
	}
	int t_n = parse_type(ctyp);
	int is_tuple = t_n != 1;

	size_t buff_size = 100;
	char *key = malloc(buff_size);
	int key_len = 0;
	if (is_tuple) {
		key[0] = '(';
		key_len++;
	}

	// used to index struct like array
	size_t byte_offset = 0;
	while (t_n --> 0) {
		// skip to beginning of next type
		while (*ctyp == '\0') ctyp++;
		// size of curr component of tuple type
		size_t typ_cmp_size;
		if (mystrcmp(ctyp, types[INT])) {
			typ_cmp_size = type_str_size[INT];
			// if adding this component results in buffer
			// overflow, resize key buffer
			if (key_len+typ_cmp_size >= buff_size) {
				buff_size += typ_cmp_size;
				buff_size *= BUFF_MULT;
				key = resize_string(key, buff_size);
			}

			// offset to multiple of type size
			unsigned int remainder = byte_offset % sizeof(int);
			if (remainder != 0)	byte_offset += sizeof(int) - remainder;

			int deref_val = ((int *)val)[byte_offset/sizeof(int)];
			byte_offset += sizeof(int);

			key_len += snprintf(key+key_len, typ_cmp_size, "%d, ", deref_val);

		} else if (mystrcmp(ctyp, types[REAL])) {
			typ_cmp_size = type_str_size[REAL];

			if (key_len+typ_cmp_size >= buff_size) {
				buff_size += typ_cmp_size;
				buff_size *= BUFF_MULT;
				key = resize_string(key, buff_size);
			}
			
			// offset to multiple of type size
			unsigned int remainder = byte_offset % sizeof(float);
			if (remainder != 0)	byte_offset += sizeof(float) - remainder;

			float deref_val = ((float *)val)[byte_offset/sizeof(float)];
			byte_offset += sizeof(float);

			key_len += snprintf(key+key_len, typ_cmp_size, "%g, ", deref_val);

		} else if (mystrcmp(ctyp, types[CHAR])) {
			typ_cmp_size = type_str_size[CHAR];

			if (key_len+typ_cmp_size >= buff_size) {
				buff_size += typ_cmp_size;
				buff_size *= BUFF_MULT;
				key = resize_string(key, buff_size);
			}
			
			// offset to multiple of type size
			unsigned int remainder = byte_offset % sizeof(char);
			if (remainder != 0)	byte_offset += sizeof(char) - remainder;

			char deref_val = ((char *)val)[byte_offset/sizeof(char)];
			byte_offset += sizeof(char);
			
			key_len += snprintf(key+key_len, typ_cmp_size, "'%c', ", deref_val);

		} else if (mystrcmp(ctyp, types[BOOL])) {
			typ_cmp_size = type_str_size[BOOL];

			if (key_len+typ_cmp_size >= buff_size) {
				buff_size += typ_cmp_size;
				buff_size *= BUFF_MULT;
				key = resize_string(key, buff_size);
			}
			
			// offset to multiple of type size
			unsigned int remainder = byte_offset % sizeof(char);
			if (remainder != 0)	byte_offset += sizeof(char) - remainder;

			char deref_val = ((char *)val)[byte_offset/sizeof(char)];
			byte_offset += sizeof(char);


			if (deref_val) {
				key_len += snprintf(key+key_len, typ_cmp_size, "%s, ", "true");
			} else {
				key_len +=snprintf(key+key_len, typ_cmp_size, "%s, ", "false");
			}
		} else if (mystrcmp(ctyp, types[STRING])) {
			// offset to multiple of type size
			unsigned int remainder = byte_offset % sizeof(char *);
			if (remainder != 0)	byte_offset += sizeof(char *) - remainder;

			char *deref_val = ((char **)val)[byte_offset/sizeof(char *)];
			byte_offset += sizeof(char *);

			typ_cmp_size = strlen(deref_val) + 3;
			if (key_len+typ_cmp_size >= buff_size) {
				buff_size += typ_cmp_size;
				buff_size *= BUFF_MULT;
				key = resize_string(key, buff_size);
			}
			
			key_len += snprintf(key+key_len, typ_cmp_size, "%s, ", deref_val);

		} else if (mystrcmp(ctyp, types[SET])) {
			struct hset_head * val_set = *(struct hset_head **)val;
			int num = HASH_COUNT(val_set);
			key = malloc(sizeof(char)*num*12+3);
			key[0] = '\0';
			strcat(key, "{");
			struct hset_head *curr, *temp;
			char *elem;
			HASH_ITER(hh, val_set, curr, temp) {
				elem = string_of(curr->val_p, "int");
				strcat(key, elem);
				strcat(key, ",");
				free(elem);
			}
			strcat(key, "}");
		}

		// skip to end of curr type
		t_l = strlen(ctyp);
		ctyp += t_l;
	}
	
  // remove ', ' from end
  key[key_len-1] = '\0';
  if (is_tuple) {
  	key[key_len-2] = ')';
  } else {
  	key[key_len-2] = '\0';
  }

	free(og_ctyp);
	return key;
}

/* inserts variable arguments into formatted string */
char *string_interpolation(char *frmt, int num, ...) {
	va_list args;
	/* initialize valist for num number of arguments */
  va_start(args, num);

  unsigned int tot_len = strlen(frmt) + 1;
  /* access all the arguments assigned to valist */
  char *curr;
  for (int i = 0; i < num; i++) {
  	curr = va_arg(args, char*);
  	tot_len += strlen(curr);
  }
  /* clean memory reserved for valist */
  va_end(args);

  va_start(args, num);
  char *ret = malloc(sizeof(char)*tot_len);
  vsprintf(ret, frmt, args);

  va_end(args);

  return ret;
}

/* frees variable arguments */
void free_args(int num, ...) {
	va_list args;
	/* initialize valist for num number of arguments */
  va_start(args, num);

  /* access all the arguments assigned to valist */
  char *curr;
  for (int i = 0; i < num; i++) {
  	free(va_arg(args, char*));
  }
	
  /* clean memory reserved for valist */
  va_end(args);
}

struct hset_head *init_hset() {
	//fprintf(stderr, "init_hset called\n");
	struct hset_head *hash_set = NULL;
	return hash_set;
}

void destroy_hset(struct hset_head *hash_set) {
	struct hset_head *curr, *temp;
	HASH_ITER(hh, hash_set, curr, temp) {
		HASH_DEL(hash_set, curr);
		free(curr->val_p);
		free(curr->val_ts);
		free(curr);
	}
}

void print_hset(struct hset_head *hash_set) {
	struct hset_head *curr, *temp;
	printf("{ ");
	HASH_ITER(hh, hash_set, curr, temp) {
		printf("%s ", curr->val_ts);
	}
	printf("}\n");
}

void *alloc_copy(void *val_p, char *typ) {
	void *new_val_p;
	if (mystrcmp(typ, types[INT])) {
		new_val_p = malloc(sizeof(int));
		memcpy(new_val_p, val_p, sizeof(int));
	}
	else if (mystrcmp(typ, types[REAL])) {
		new_val_p = malloc(sizeof(double));
		memcpy(new_val_p, val_p, sizeof(double));
	} 
	else if (mystrcmp(typ, types[CHAR])) {
		new_val_p = malloc(sizeof(char));
		memcpy(new_val_p, val_p, sizeof(char));
	} 
	else if (mystrcmp(typ, types[BOOL])) { 
		new_val_p = malloc(sizeof(int));
		memcpy(new_val_p, val_p, sizeof(int));
	}

	return new_val_p;
}

struct hset_head *copy_hset(struct hset_head *hash_set, char* typ) {
	struct hset_head *hset_new = init_hset();
	struct hset_head *curr, *copied, *temp;
	void *new_val_p, *new_val_ts;
	HASH_ITER(hh, hash_set, curr, temp) {
		copied = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(curr->val_p, typ);
		new_val_ts = strdup(curr->val_ts);
		copied->val_ts = new_val_ts;
		copied->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hset_new, (copied->val_ts), strlen(copied->val_ts), copied);
	}
	return hset_new;
}

struct hset_head *_add_val(char *val_ts, void *val_p, 
						  char *typ, struct hset_head *hash_set) {
	struct hset_head *temp;
	void *new_val_p, *new_val_ts;
	HASH_FIND(hh, hash_set, val_ts, strlen(val_ts), temp);
	if (temp == NULL) {
		//fprintf(stderr, "_add_val, key does not exist\n");
		temp = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(val_p, typ);
		temp->val_ts = val_ts;
		temp->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hash_set, (temp->val_ts), strlen(temp->val_ts), temp);
		//fprintf(stderr, "_add_val, key %s added\n", val_ts);
	}
	return hash_set;
}

struct hset_head *add_val(void *val, char *typ, struct hset_head *hash_set) {
	//fprintf(stderr, "add_val called\n");
	struct hset_head *hset_new = copy_hset(hash_set, typ);
	char *key = string_of(val, typ);
	//fprintf(stderr, "add_val, key %s insert\n", key);
	return _add_val(key, val, typ, hset_new);
}

struct hset_head *_del_val(char *val_ts, char *typ, struct hset_head *hash_set) {
	/* 
	 * copy hash_set over; val_p can be NULL as:
	 * a) key exists and will not be added
	 * b) key added with NULL value and will be deleted when found
	 */
	struct hset_head *temp;
	HASH_FIND(hh, hash_set, val_ts, strlen(val_ts), temp);

	if (temp != NULL) {
		HASH_DEL(hash_set, temp);
		free(temp->val_p);
		free(temp->val_ts);
		free(temp);
	}
	return hash_set;
}

struct hset_head *del_val(void *val, char *typ, struct hset_head *hash_set) {
	struct hset_head *hset_new = copy_hset(hash_set, typ);
	char *key = string_of(val, typ);
	struct hset_head *temp = _del_val(key, typ, hset_new);
	free(key);
	return temp;
}

struct hset_head *hset_union(struct hset_head *left, struct hset_head *right, char *typ) {
	struct hset_head *hset_new = copy_hset(left, typ);
	//fprintf(stderr, "in union, copied: ");
	//print_hset(hset_new);
	//fprintf(stderr, "in union, to copy: ");
	//print_hset(right);
	struct hset_head *curr, *temp;
	HASH_ITER(hh, right, curr, temp) {
		//fprintf(stderr, "in union, adding %s\n", curr->val_ts);
		_add_val(curr->val_ts, curr->val_p, typ, hset_new);
	}
	return hset_new;
}


struct hset_head *hset_diff(struct hset_head *left, struct hset_head *right, char *typ) {
	struct hset_head *hset_new = copy_hset(left, typ);
	struct hset_head *curr, *temp;
	HASH_ITER(hh, right, curr, temp) {
		hset_new = _del_val(curr->val_ts, typ, hset_new);	
	}
	return hset_new;
}

// int main() {
// 	// int x = 4;
// 	// float d = 3.14;
// 	// char a = 'a';
// 	// printf("%s\n", string_of(&x, "int"));
// 	// printf("%s\n", string_of(&d, "real"));
// 	// printf("%s\n", string_of(&a, "char"));
// 	// printf("%s\n", string_of(&x, "bool"));
// 	struct {int x; float y; char *z; char w;} s = {3,4.3, "hello", '9'};
// 	printf("%s\n", string_of(&s, "(int * real * string * char)"));
// }