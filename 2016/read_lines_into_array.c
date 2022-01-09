#include <stdlib.h> // for read_lines_into_array which is using malloc etc.


char **read_lines_into_array(const char *filename) {
    int lines_allocated = 128;
    int max_line_len = 100;

    /* Allocate lines of text */
    // allocate char* size TIMES(*) lines_allocated
    char **lines = (char **)malloc(sizeof(char*)*lines_allocated);
    if (lines==NULL)
        {
        fprintf(stderr,"Out of memory (1).\n");
        exit(1);
        }

    FILE *fp = fopen(filename, "r");
    if (fp == NULL)
        {
        fprintf(stderr,"Error opening file.\n");
        exit(2);
        }

    int i;
    for (i=0;1;i++)
        {
        int j;

        /* Have we gone over our line allocation? */
        if (i >= lines_allocated)
            {
            int new_size;

            /* Double our allocation and re-allocate */
            new_size = lines_allocated*2;
            lines = (char **)realloc(lines,sizeof(char*)*new_size);
            if (lines==NULL)
                {
                fprintf(stderr,"Out of memory.\n");
                exit(3);
                }
            lines_allocated = new_size;
            }
        /* Allocate space for the next line */
        lines[i] = malloc(max_line_len);
        if (lines[i]==NULL)
            {
            fprintf(stderr,"Out of memory (3).\n");
            exit(4);
            }
        if (fgets(lines[i],max_line_len-1,fp)==NULL)
            break;

        /* Get rid of CR or LF at end of line */
        for (j=strlen(lines[i])-1;j>=0 && (lines[i][j]=='\n' || lines[i][j]=='\r');j--)
            ;
        lines[i][j+1]='\0';
        }
    /* Close file */
    fclose(fp);

    // was in main() before so example code printed lines
    // and then freed the memory
    // int j;
    // for(j = 0; j < i; j++)
    //     printf("%s\n", lines[j]);

    /* Good practice to free memory */
    // for (;i>=0;i--)
    //     free(lines[i]);
    // free(lines);

    return lines;
}
