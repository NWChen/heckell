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
	STRING, SET, MAP
};

// max num of char in string of type
// -1 means should not be used
// includes 2 char padding
int type_str_size[] = {
	14, 100, 6, 8, -1, -1, -1
};

char *types[] = {
	"int", "real", "char", "bool", "string", "set", "map"
};

struct hset_head {
	char *val_ts;
	void *val_p;
	//char *typ;
	UT_hash_handle hh;
};


// **********************************************
//
//	String manipulation functions
//
// ********************************************** 

int mystrcmp(const char *str1, const char *str2) {
	return strcasecmp(str1, str2) == 0;
}

/* Return number of elements in tuple, divide typ with null
 * characters
 */
int parse_type(char *typ) {
	int n = 1;
	int p = 0;
	int curr_s = 0;
	int len = strlen(typ);
	int i;
	for (i = 0; i < len; i++) {
		char c = typ[i];
		if (c == ')') {
			p--;
			if (p == 0) {
				// only trim paren of element if it is a tuple
				// else, restore the beginning paren (e.g. sets)
				if (i+2 >= len || typ[i+2] == '*') typ[i] = '\0';
				else typ[curr_s] = '(';
			}
			continue;
		} else if (c == '(') {
			if (p == 0) typ[i] = '\0';
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
			curr_s = i+2;
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
	key[0] = '\0';
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
		t_l = strlen(ctyp);
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

		} else if (mystrcmp(ctyp+t_l-3, types[SET]) || 
							 mystrcmp(ctyp+t_l-3, types[MAP])) {
			char *temp_ctyp = strdup(ctyp);
			// remove ' set' from end of type str
			temp_ctyp[t_l-4] = '\0';

			// check if '{}, ' can be added
			typ_cmp_size = 4;
			if (key_len+typ_cmp_size >= buff_size) {
				buff_size += typ_cmp_size;
				buff_size *= BUFF_MULT;
				key = resize_string(key, buff_size);
			}
			strcat(key, "{");
			key_len++;

			struct hset_head * val_set = *(struct hset_head **)val;
			int num = HASH_COUNT(val_set);

			struct hset_head *curr, *temp;
			char *elem;
			HASH_ITER(hh, val_set, curr, temp) {
				elem = string_of(curr->val_p, temp_ctyp);

				typ_cmp_size = strlen(elem) + 4;
				if (key_len+typ_cmp_size >= buff_size) {
					buff_size += typ_cmp_size;
					buff_size *= BUFF_MULT;
					key = resize_string(key, buff_size);
				}

				strcat(key, elem);
				strcat(key, ", ");
				free(elem);
				key_len += typ_cmp_size - 2;
			}

			if (num != 0) {
				// remove leading ', ' and replace with '}, '
				key[key_len-2] = '}';
				key[key_len-1] = ',';
				key[key_len] = ' ';
				key[key_len+1] = '\0';
				key_len++;
			} else {
				strcat(key, "}, ");
				key_len += 3;
			}

			free(temp_ctyp);
		}

		// skip to end of curr type
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
  int i;
  for (i = 0; i < num; i++) {
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


// **********************************************
//
//	Memory related functions
//
// ********************************************** 

/* frees variable arguments */
void free_args(int num, ...) {
	va_list args;
	/* initialize valist for num number of arguments */
  va_start(args, num);

  /* access all the arguments assigned to valist */
  char *curr;
  int i;
  for (i = 0; i < num; i++) {
  	free(va_arg(args, char*));
  }
	
  /* clean memory reserved for valist */
  va_end(args);
}

/* Get size in bytes of type */
int size_of_type(char *typ) {
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

	// used to index struct like array
	size_t typ_size = 0;
	size_t max_size = 0;
	while (t_n --> 0) {
		// skip to beginning of next type
		while (*ctyp == '\0') ctyp++;

		if (mystrcmp(ctyp, types[INT])) {
			size_t m_size = sizeof(int);
			unsigned int remainder = typ_size % m_size;
			if (remainder != 0)	typ_size += m_size - remainder;
			typ_size += m_size;
			if (max_size < m_size) max_size = m_size;
		}
		else if (mystrcmp(ctyp, types[REAL])) {
			size_t m_size = sizeof(float);
			unsigned int remainder = typ_size % m_size;
			if (remainder != 0)	typ_size += m_size - remainder;
			typ_size += m_size;
			if (max_size < m_size) max_size = m_size;
		} 
		else if (mystrcmp(ctyp, types[CHAR])) {
			size_t m_size = sizeof(char);
			unsigned int remainder = typ_size % m_size;
			if (remainder != 0)	typ_size += m_size - remainder;
			typ_size += m_size;
			if (max_size < m_size) max_size = m_size;
		} 
		else if (mystrcmp(ctyp, types[BOOL])) { 
			size_t m_size = sizeof(int);
			unsigned int remainder = typ_size % m_size;
			if (remainder != 0)	typ_size += m_size - remainder;
			typ_size += m_size;
			if (max_size < m_size) max_size = m_size;
		} else if (mystrcmp(ctyp, types[STRING])) {
			size_t m_size = sizeof(char *);
			unsigned int remainder = typ_size % m_size;
			if (remainder != 0)	typ_size += m_size - remainder;
			typ_size += m_size;
			if (max_size < m_size) max_size = m_size;
		}

		// skip to end of curr type
		t_l = strlen(ctyp);
		ctyp += t_l;
	}

	unsigned int remainder = typ_size % max_size;
	if (remainder != 0)	typ_size += max_size - remainder;

	free(og_ctyp);

	return typ_size;
}

void *alloc_copy(void *val_p, char *typ) {
	int size = size_of_type(typ);

	void *new_val_p = malloc(size);
	memcpy(new_val_p, val_p, size);

	return new_val_p;
}


// **********************************************
//
//	HashSet functions
//
// ********************************************** 

struct hset_head *init_hset() {
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
		temp = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(val_p, typ);
		temp->val_ts = val_ts;
		temp->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hash_set, (temp->val_ts), strlen(temp->val_ts), temp);
	}
	return hash_set;
}

struct hset_head *add_val(void *val, char *typ, struct hset_head *hash_set) {
	struct hset_head *hset_new = copy_hset(hash_set, typ);
	char *key = string_of(val, typ);

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
	
	struct hset_head *curr, *temp;
	HASH_ITER(hh, right, curr, temp) {
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


/* Get value from map hset using key of type typ
 */
void *find_val(struct hset_head *hash_set, void *key_val, char *typ) {
	char *key = string_of(key_val, typ);
	struct hset_head *temp;
	HASH_FIND(hh, hash_set, key, strlen(key), temp);
	
	if (temp == NULL) {
		fprintf(stderr, "Key not in map: %s\n", key);
		exit(EXIT_FAILURE);
	}

	free(key);
	return temp->val_p;
}

/* Create hash set from variable number of n arguments
 * all args must be of type typ. Can specify if values
 * needs to be stored like arrays
 */
struct hset_head *hset_from_list(char *typ, int is_map, int n, ...) {
	struct hset_head *hash_set = init_hset();

	va_list args;
	/* initialize valist for num number of arguments */
  va_start(args, n);

  char *ctyp = strdup(typ);
  char *og_ctyp = ctyp;

  // trim parentheses
  int t_l = strlen(ctyp);
	if (ctyp[t_l-1] == ')') {
		ctyp[t_l-1] = '\0';
		ctyp++;
	}

  if (is_map) {
  	// parse_type separates elements of tuple
  	// by inserting null chars, effectively trimming
  	// the second type of this tuple
  	parse_type(ctyp);
  }

  /* access all the arguments assigned to valist */
  char *curr;
  int i;
  for (i = 0; i < n; i++) {
  	curr = va_arg(args, void*);

  	char *key = string_of(curr, ctyp);
  	hash_set = _add_val(key, curr, typ, hash_set);
  }
  /* clean memory reserved for valist */
  va_end(args);
  free(og_ctyp);

	return hash_set;
}


int hset_len(struct hset_head *hash_set) {
	return HASH_COUNT(hash_set);
}

void *get_next(struct hset_head *curr) {	
	return (curr != NULL) ? (void *) curr->hh.next : NULL;
}

void *get_val(struct hset_head *curr) {
	return curr->val_p;
}

int has_key(struct hset_head *hash_set, void *val_p, char *typ) {
	char *val_ts = string_of(val_p, typ);
	struct hset_head *temp;
	HASH_FIND(hh, hash_set, val_ts, strlen(val_ts), temp);

	free(val_ts);
	return (temp != NULL);
}