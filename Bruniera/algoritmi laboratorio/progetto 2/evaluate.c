#include "trees.h"

void timespec_dif(const struct timespec *a, const struct timespec *b, struct timespec *result);
void timespec_sum(const struct timespec *a, const struct timespec *b, struct timespec *result);
void timespec_mul(const struct timespec *a,  int b, struct timespec *result);
void timespec_div(const struct timespec *a,  int b, struct timespec *result);
void timespec_getres(struct timespec *res);

int main(int argc, char** argv) {
    char command[7];
    char val[256];
    int key;
    bool run = true;
    Node *tree;
    AvlNode *avl_tree;
    RbtNode *rbt_tree;
    tree = NULL;
    
    //parsing ed esecuzione del comando, ogni comando corrisponde ad una sola funzione (escluso l'eventuale tree=NULL)
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%d %s", &key, &val[0]);
            insert(key, val, &tree);
        } else if(strcmp(command, "clear") == 0) {
            clear(tree);
            tree = NULL;
        } else if(strcmp(command, "find") == 0) {
            scanf("%d", &key);
            printf("%s\n", find(tree, key));
        } else if(strcmp(command, "show") == 0) {
            show(tree);
            puts("");
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    //parsing ed esecuzione del comando, ogni comando corrisponde ad una sola funzione (escluso l'eventuale tree=NULL)
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%d %s", &key, (char*)&val);
            avl_insert(key, val, &avl_tree);
        } else if(strcmp(command, "clear") == 0) {
            avl_clear(avl_tree);
            tree = NULL;
        } else if(strcmp(command, "find") == 0) {
            scanf("%d", &key);
            printf("%s\n", avl_find(avl_tree, key));
        } else if(strcmp(command, "show") == 0) {
            avl_show(avl_tree);
            puts("");
        } else if(strcmp(command, "height") == 0) {
            printf("%d\n", height(avl_tree));
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    //parsing ed esecuzione del comando, ogni comando corrisponde ad una sola funzione (escluso l'eventuale tree=NULL)
    while(run){
        scanf("%s", (char*)&command);
        
        if(strcmp(command, "insert") == 0) {
            scanf("%d %s", &key, (char*) val);
            rbt_insert(key, val, &rbt_tree);
        } else if(strcmp(command, "clear") == 0) {
            rbt_clear(rbt_tree);
            tree = NULL;
        } else if(strcmp(command, "find") == 0) {
            scanf("%d", &key);
            printf("%s\n", rbt_find(rbt_tree, key));
        } else if(strcmp(command, "show") == 0) {
            rbt_show(rbt_tree);
            puts("");
        } else if(strcmp(command, "exit") == 0) {
            run = false;
        }
    }
    
    return 0;
}

//funzione per la sottrazione tra timespec adattata dalla macro timeval_diff_macro in sys/time.h
void timespec_dif(const struct timespec *a, const struct timespec *b, struct timespec *result) {
    result->tv_sec = a->tv_sec - b->tv_sec;
    result->tv_nsec = a->tv_nsec - b->tv_nsec;
    if (result->tv_nsec < 0) {
        result->tv_sec--;
        result->tv_nsec += 1000000000L;
    }
}

//somma tra timespec adattata da timespec_dif
void timespec_sum(const struct timespec *a, const struct timespec *b, struct timespec *result) {
    result->tv_sec  = a->tv_sec + b->tv_sec;
    result->tv_nsec = a->tv_nsec + b->tv_nsec;
    if (result->tv_nsec > 1000000000L) {
        result->tv_sec++;
        result->tv_nsec -= 1000000000L;
    }
}

//moltiplicazione per intero di timespec. (a+b/1E9)*c = c*a+cb/1E9
void timespec_mul(const struct timespec *a, int b, struct timespec *result) {
    long long temp = (long long) b * (long long) a->tv_nsec;
    result->tv_sec = (a->tv_sec * b) + (temp / 1000000000LL);
    result->tv_nsec = temp % 1000000000LL;
}

//divisione per intero di timespec. (a+b/1E9)/c = a/c+b/cE9
void timespec_div(const struct timespec *a, int b, struct timespec *result) {
    long long temp = (((long long) (a->tv_sec % b) * 1000000000LL) + (long long) a->tv_nsec) / (long long) b;
    result->tv_sec = (a->tv_sec / b) + (temp / 1000000000LL);
    result->tv_nsec = temp % 1000000000LL;
}
