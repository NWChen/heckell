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

int main(){
	return 0;
}