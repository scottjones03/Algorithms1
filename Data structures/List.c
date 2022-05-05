#include<stdio.h>
#include<stdlib.h>
int main(void)
{
    char counter;

    struct listWagon
    {
        int payload;  
        struct listWagon *next;

    };


    struct listWagon *head = NULL;
    struct listWagon *node = malloc(sizeof(*node));
    node->next = head;
    head=node;
}