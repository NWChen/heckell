#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "uthash.h"

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


char *string_of(void *val, char *typ) {
	char *key;
	if (mystrcmp(typ, types[INT])) {
		key = malloc(sizeof(char)*12);
		//fprintf(stderr, "string_of, int type\n");
		//fprintf(stderr, "string_of, write %d to key\n", *(int *)val);
		snprintf(key, sizeof(char)*12, "%d", *(int *)val);
	} else if (mystrcmp(typ, types[REAL])) {
		key = malloc(sizeof(char)*100);
		snprintf(key, sizeof(char)*100, "%g", *(double *)val);

	} else if (mystrcmp(typ, types[CHAR])) {
		key = malloc(sizeof(char)*4);
		snprintf(key, sizeof(char)*4, "%c", *(char *)val);
		fprintf(stderr, "string_of, int type\n");
		fprintf(stderr, "string_of, write %d to key\n", *(int *)val);

	} else if (mystrcmp(typ, types[BOOL])) {
		key = malloc(sizeof(char)*6);
		if (*(int *)val) {
			snprintf(key, sizeof(char)*5, "%s", "true");
		} else {
			snprintf(key, sizeof(char)*6, "%s", "false");
		}
	} else if (mystrcmp(typ, types[STRING])) {
		key = strdup(*(char **)val);

	} else if (mystrcmp(typ, types[SET])) {
		struct hset_head * val_set = *(struct hset_head **)val;
		int num = HASH_COUNT(val_set);
		key = malloc(sizeof(char)*num*11+1);
		key[0] = '\0';
		struct hset_head *curr, *temp;
		char *elem;
		HASH_ITER(hh, val_set, curr, temp) {
			elem = string_of(curr->val_p, "int");
			strcat(key, elem);
			strcat(key, ",");
			free(elem);
		}
	}
	//fprintf(stderr, "string_of, return %s key\n", key);
	return key;
}


struct hset_head *init_hset() {
	//fprintf(stderr, "init_hset called\n");
	struct hset_head *hash_set = NULL;
	return hash_set;
}

void *alloc_copy(void *val_p, char *typ) {
	void *new_val_p;
	if (mystrcmp(typ, "Int")) {
		//fprintf(stderr, "alloc_copy, int type\n");
		new_val_p = malloc(sizeof(int));
		//fprintf(stderr, "alloc_copy, malloc success\n");
		//fprintf(stderr, "alloc_copy, memcpy from %p\n", val_p);
		//fprintf(stderr, "alloc_copy, memcpy %d\n", *(int *)val_p);
		memcpy(new_val_p, val_p, sizeof(int));
	}
	else if (mystrcmp(typ, "Real")) {
		new_val_p = malloc(sizeof(double));
		memcpy(new_val_p, val_p, sizeof(double));
	} 
	else if (mystrcmp(typ, "Char")) {
		new_val_p = malloc(sizeof(char));
		fprintf(stderr, "alloc_copy, memcpy from %p\n", val_p);
		fprintf(stderr, "alloc_copy, memcpy %c\n", *(char *)val_p);
		memcpy(new_val_p, val_p, sizeof(char));
	} 
	else { // bool type
		new_val_p = malloc(sizeof(int));
		memcpy(new_val_p, val_p, sizeof(int));
	}
	//fprintf(stderr, "alloc_copy, new pointer returned");
	return new_val_p;
}


struct hset_head *_add_val(char *val_ts, void *val_p, 
						  char *typ, struct hset_head *hash_set) {
	struct hset_head *hset_new = init_hset();
	struct hset_head *curr, *copied, *temp;
	void *new_val_p, *new_val_ts;
	fprintf(stderr, "_add_val, copy to new hashset");
	HASH_ITER(hh, hash_set, curr, temp) {
		copied = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(curr->val_p, typ);
		new_val_ts = strdup(curr->val_ts);
		copied->val_ts = new_val_ts;
		copied->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hset_new, (copied->val_ts), strlen(copied->val_ts), copied);
	}

	HASH_FIND(hh, hset_new, val_ts, strlen(val_ts), temp);
	if (temp == NULL) {
		fprintf(stderr, "_add_val, key does not exist");
		temp = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(val_p, typ);
		temp->val_ts = val_ts;
		temp->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hset_new, (temp->val_ts), strlen(temp->val_ts), temp);
		fprintf(stderr, "_add_val, key %s added\n", val_ts);
	}
	return hset_new;
}

struct hset_head *add_val(void *val, char *typ, struct hset_head *hash_set) {
	//fprintf(stderr, "add_val called\n");
	char *key = string_of(val, typ);
	//fprintf(stderr, "add_val, key %s insert\n", key);
	return _add_val(key, val, typ, hash_set);
}


struct hset_head *_del_val(char *val_ts, char *typ, struct hset_head *hash_set) {
	/* 
	 * copy hash_set over; val_p can be NULL as:
	 * a) key exists and will not be added
	 * b) key added with NULL value and will be deleted when found
	 */
	struct hset_head *hset_new = _add_val(val_ts, (void *)NULL, typ, hash_set);
	struct hset_head *temp;
	HASH_FIND(hh, hset_new, val_ts, strlen(val_ts), temp);

	if (temp != NULL) {
		HASH_DEL(hset_new, temp);
		free(temp->val_p);
		free(temp->val_ts);
		free(temp);
	}
	return hset_new;
}

struct hset_head *del_val(void *val, char *typ, struct hset_head *hash_set) {
	char *key = string_of(val, typ);
	struct hset_head *temp = _del_val(key, typ, hash_set);
	free(key);
	return temp;
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

#ifdef BUILD_TEST
int main() {
	int zero = 0;
	int four = 4;
	int eight = 8;
	int ten = 10;
	struct hset_head *hset1 = init_hset();
	struct hset_head *hset2 = add_val((void *)&four, "int", hset1);
	printf("2: ");
	print_hset(hset2);
	struct hset_head *hset3 = add_val((void *)&zero, "int", hset2);
	printf("3: ");
	print_hset(hset3);
	struct hset_head *hset4 = add_val((void *)&ten, "int", hset3);
	printf("4: ");
	print_hset(hset4);
	struct hset_head *hset5 = del_val((void *)&four, "int", hset4);
	printf("5: ");
	print_hset(hset5);
	printf("6: ");
	struct hset_head *hset6 = add_val((void *)&hset4, "set", hset4);
	print_hset(hset6);
	destroy_hset(hset1);
	destroy_hset(hset2);
	destroy_hset(hset3);
	destroy_hset(hset4);
	destroy_hset(hset5);
	destroy_hset(hset6);

	return 0;
}
#endif