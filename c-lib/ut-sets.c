#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "uthash.h"

struct hset_head {
	char *val_ts;
	void *val_p;
	//char *typ;
	UT_hash_handle hh;
};

struct hset_head *init_hset() {
	struct hset_head *hash_set = NULL;
	return hash_set;
}

void *alloc_copy(void *val_p, char *typ) {
	void *new_val_p;
	if (strcmp(typ, "Int")) {
		new_val_p = malloc(sizeof(int));
		memcpy(new_val_p, val_p, sizeof(int));
	}
	else if (strcmp(typ, "Real")) {
		new_val_p = malloc(sizeof(float));
		memcpy(new_val_p, val_p, sizeof(float));
	} 
	else if (strcmp(typ, "Char")) {
		new_val_p = malloc(sizeof(char));
		memcpy(new_val_p, val_p, sizeof(char));
	} 
	else { // bool type
		new_val_p = malloc(1);
		memcpy(new_val_p, val_p, 1);
	}
	return new_val_p;
}
struct hset_head *add_val(char *val_ts, void *val_p, 
						  char *typ, struct hset_head *hash_set) {
	struct hset_head *hset_new = init_hset();
	struct hset_head *curr, *copied, *temp;
	void *new_val_p;
	HASH_ITER(hh, hash_set, curr, temp) {
		copied = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(curr->val_p, typ);
		copied->val_ts = curr->val_ts;
		copied->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hset_new, (copied->val_ts), strlen(copied->val_ts), copied);
	}

	HASH_FIND(hh, hset_new, val_ts, strlen(val_ts), temp);
	if (temp == NULL) {
		temp = malloc(sizeof(struct hset_head));
		new_val_p = alloc_copy(val_p, typ);
		temp->val_ts = val_ts;
		temp->val_p = new_val_p;
		HASH_ADD_KEYPTR(hh, hset_new, (temp->val_ts), strlen(temp->val_ts), temp);
	}
	return hset_new;
}

struct hset_head *del_val(char *val_ts, char *typ, struct hset_head *hash_set) {
	/* 
	 * copy hash_set over; val_p can be NULL as:
	 * a) key exists and will not be added
	 * b) key added with NULL value and will be deleted when found
	 */
	struct hset_head *hset_new = add_val(val_ts, (void *)NULL, typ, hash_set);
	struct hset_head *temp;
	HASH_FIND(hh, hset_new, val_ts, strlen(val_ts), temp);

	if (temp != NULL) {
		HASH_DEL(hset_new, temp);
		free(temp->val_p);
		free(temp);
	}
	return hset_new;
}

void destroy_hset(struct hset_head *hash_set) {
	HASH_ITER(hh, hash_set, curr, temp) {
		HASH_DEL(hash_set, curr);
		free(curr->val_p);
		free(curr);
	}
}

void print_hset(struct hset_head *hash_set) {
	struct hset_head *curr, *temp;
	printf("hash set contains: ");
	HASH_ITER(hh, hash_set, curr, temp) {
		printf("%s ", curr->val_ts);
	}
	printf("\n");
}

int main() {
	int four = 4;
	int eight = 8;
	int ten = 10;
	struct hset_head *hset1 = init_hset();
	struct hset_head *hset2 = add_val("4", (void *)&four, "Int", hset1);
	print_hset(hset2);
	struct hset_head *hset3 = add_val("8", (void *)&eight, "Int", hset2);
	struct hset_head *hset4 = add_val("10", (void *)&ten, "Int", hset3);
	print_hset(hset4);
	struct hset_head *hset5 = del_val("4", "Int", hset4);
	print_hset(hset5);
	destroy_hset(hset1);
	destroy_hset(hset2);
	destroy_hset(hset3);
	destroy_hset(hset4);
	destroy_hset(hset5);

	return 0;
}