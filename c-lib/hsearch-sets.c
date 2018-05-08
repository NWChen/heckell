#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#define __USE_GNU
#include <search.h>
#define DEFAULT_MAX 100

typedef struct hset {
	struct hsearch_data values;
	//size_t size;
	char *typ;
} hset;

hset *init_hset(char *typ) {
	hset *hash_set = malloc(sizeof(hset));
	hash_set->values = (struct hsearch_data) {0};
	//hash_set->size = 0;
	hash_set->typ = typ;
	
	hcreate_r(DEFAULT_MAX, &hash_set->values);
	return hash_set;
}

/*
	TODO: for each addition/change, new table is generated
	malloc-ed space is not copied, but means many many pointers
*/
hset *insert_val(char *val_ts, void *val_p, hset *hash_set) {
	hset *new_hset = init_hset(hash_set->typ);
	/* 
	TODO: is this safe? 
	the pointer values will never be updated
	what happens when hdestroy 
	temp solution: wait till end to destroy tables
	*/
	memcpy(&new_hset->values, &hash_set->values, sizeof(hash_set->values));

	void *new_val_p;
	if (strcmp(hash_set->typ, "Int")) {
		new_val_p = malloc(sizeof(int));
		memcpy(new_val_p, val_p, sizeof(int));
	}
	else if (strcmp(hash_set->typ, "Real")) {
		new_val_p = malloc(sizeof(float));
		memcpy(new_val_p, val_p, sizeof(float));
	} 
	else if (strcmp(hash_set->typ, "Char")) {
		new_val_p = malloc(sizeof(char));
		memcpy(new_val_p, val_p, sizeof(char));
	} 
	else { // bool type
		new_val_p = malloc(1);
		memcpy(new_val_p, val_p, 1);
	}

	ENTRY e, *e_p;
	e.key = val_ts;
	e.data = new_val_p;

	//TODO: error checking, copying to larger struct
	hsearch_r(e, ENTER, &e_p, &new_hset->values);
	return new_hset;
}

void *get_val(char *key, hset *hash_set) {
	ENTRY e, *ep;
	e.key = key;
	if (!hsearch_r(e, FIND, &ep, &hash_set->values)) {
		return NULL;
	}

	return ep->data;
}

hset *del_val(char *key, hset *hash_set) {
	hset *new_hset = init_hset(hash_set->typ);
	/* 
	TODO: is this safe? 
	the pointer values will never be updated
	what happens when hdestroy 
	temp solution: wait till end to destroy tables
	*/
	memcpy(&new_hset->values, &hash_set->values, sizeof(hash_set->values));
	void *ret_val = get_val(key, new_hset);
	printf("initial ret_val: %d\n", *((int *)ret_val));

	ENTRY e, *e_p;

	//TODO: error checking, copying to larger struct
	if (hsearch_r(e, ENTER, &e_p, &new_hset->values))
		e_p->data = (void *) NULL;
	ret_val = get_val(key, new_hset);
	if (ret_val == NULL)
		printf("successful delete\n");
	else
		printf("new ret_val: %d\n", *(int *)ret_val);	
  	return new_hset;
}

void destroy_hset(hset *hash_set) {
	hdestroy_r(&hash_set->values);
	free(hash_set);
	hash_set = NULL;
}

int main(){
	void *ret_val;
	int four = 4;
	int eight = 8;


	hset *hashset = init_hset("Int");
	hset *hashset2 = insert_val("4", (void *)&four, hashset);
	ret_val = get_val("4", hashset2);
	printf("hset2 val: %d\n", *((int *)ret_val));
	hset *hashset3 = insert_val("8", (void *)&eight, hashset2);
	ret_val = get_val("4", hashset3);
	printf("hset3 val: %d\n", *((int *)ret_val));
	ret_val = get_val("8", hashset3);
	printf("hset3 val: %d\n", *((int *)ret_val));

	hset *hashset4 = del_val("4", hashset2);
	ret_val = get_val("4", hashset2);
	printf("hset2 val: %d\n", *((int *)ret_val));
	ret_val = get_val("4", hashset4);
	if (ret_val)
		printf("hset4 val: %d\n", *((int *)ret_val));
	else
		printf("hset4 val: NULL");
	//destroy_hset(hashset3);


	return 0;
}