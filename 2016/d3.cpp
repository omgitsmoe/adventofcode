#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


#define LINE_BUFFSIZE 256
int countlines(const char *filename) {
	FILE *file = fopen(filename, "r");
	if (!file) {
		printf("File couldnt be loaded!");
		return(-1);
	}

	int lines = 0;
	char line[LINE_BUFFSIZE];

	// doesnt get whole line if its longer than LINE_BUFFSIZE
	while (fgets((char *)line, sizeof(line), file)) {
		// -> just get contents of line till we reach \n overwriting previous stuff
		while (strlen(line)+1 == LINE_BUFFSIZE && !(line[LINE_BUFFSIZE - 2] == '\n'))
			fgets((char *)line, sizeof(line), file);
		
		lines++;
	}

    	fclose(file);

	return lines;
}


struct read_textfile_result {
	unsigned int line_count;
	char **lines;

	~read_textfile_result();
};

read_textfile_result::~read_textfile_result() {
	//TODO free memory
}

bool read_text_file_into_buffer(const char *filename, read_textfile_result *buffer) {
	int lines_allocated = 100;
	// cant use new, since realloc only works for memory allocated by c/malloc
	char **lines = (char **)malloc(sizeof(char *) * lines_allocated);
	if (lines == NULL) {
		printf("Out of memory!\n");
		return(NULL);
	}

	FILE *file = fopen(filename, "r");
	// check for NULL or doesn !file work?
	if (!file) {
		printf("File couldnt be loaded!\n");
		return(NULL);
	}

	// 100 means it can store 99 chars + \0
	int line_max_chars = 100;
	int line_index;
	for (line_index=0;1;++line_index) {
		// assign the returned ptr from malloc directly to the array?
		char *line = (char *)malloc(sizeof(char) * line_max_chars);
		// have we reached our limit of allocated lines?
		if (line_index >= lines_allocated) {
			// reallocate
			lines_allocated = lines_allocated*2;
			lines = (char **)realloc(lines,sizeof(char *) * lines_allocated);
			if (lines==NULL) {
				printf("Out of memory.\n");
				return(NULL);
			}
		}
		if (fgets(line, line_max_chars, file) == NULL)
			break;
		/* note that fgets don't strip the terminating \n, checking its
		   presence would allow to handle lines longer that sizeof(line) */
		// if line (incl \n) isnt exactly line_max_chars chars long \n wont be the last
		// element of array -> check if array completely filled first then check if
		// one b4 last is \n (\0 will be all free slots in the char array and the last char
		// of the string which terminates it) -> so check for char before last to be != \0
		// since last is always \0)
		// if i dont use calloc memory isnt intialized to zero so i cant check \0
		int new_line_size = line_max_chars;
		// strlen returns length of byte string not incl \0 sizeof returns lenght with \0
		// checking for != '\n' will also be true for garbage that is still in memory
		// -> better to check for !( == '\n')
		while (strlen(line)+1 == new_line_size && !(line[new_line_size - 2] == '\n')) {
			printf("Line was too large for buffer size of %d\n", new_line_size);
			// free_chars always equals old value of new_line_size
			// also is amount of chars currently occupied in array
			int free_chars = new_line_size;
			// reallocate line buffer and reapeat till we got the whole line
			new_line_size = new_line_size * 2;
			// does it work without casting to char*?
			line = (char *)realloc(line, sizeof(char) * new_line_size);
			// part of the line is now already in the buffer
			// pass pointer to current end of str in line to fgets
			// also pass appropriate amount of chars to get since buffer is partly filled
			// new_line_size-line_max_chars only correct amount of chars on first iter
			// free_chars excluding last \0 of prev buffer
			// free_chars always equals old value of new_line_size
			// int free_chars = new_line_size - new_line_size/2;
			// line + new_line_size - 1 points to the null termintor so the last element of
			// the char array -> line + 98 -> 99th char
			fgets(line + free_chars - 1, free_chars + 1, file);
		}
		// TODO strip \r \n
		// assign directly instead of doing it on stack first?
		lines[line_index] = line;
	}
	/* may check feof here to make a difference between eof and io failure -- network
	   timeout for instance */
	if (feof(file)) {
		printf("End of file reached -> read succesfully!\n");
	} else {
		printf("Failed to read file %s!", filename);
		return(NULL);
	}

	buffer->line_count = line_index;
	// better to work on it directly?
	buffer->lines = lines;

    	fclose(file);
	return (1);
}


char **read_text_file_into_buffer(const char *filename) {
	// 2 passes -> using reallocation would be faster
	int lines_file = countlines(filename);
	char **lines = new char *[lines_file];
	FILE *file = fopen(filename, "r");
	if (!file) {
		printf("File couldnt be loaded!");
		return(NULL);
	}

	char *line = new char[LINE_BUFFSIZE];
	int line_index = 0;
	while (fgets(line, LINE_BUFFSIZE, file)) {
		/* note that fgets don't strip the terminating \n, checking its
		   presence would allow to handle lines longer that sizeof(line) */
		lines[line_index] = line;
		assert(line_index < lines_file);
		// if line (incl \n) isnt exactly LINE_BUFFSIZE chars long \n wont be the last
		// element of array -> check if array completely filled first then check if
		// one b4 last is \n (\0 will be all free slots in the char array and the last char
		// of the string which terminates it) -> so check for char before last to be != \0
		// since last is always \0)
		if (line[LINE_BUFFSIZE - 2] != '\0' && line[LINE_BUFFSIZE - 2] != '\n') {
			printf("Line was too large for buffer size of %d\n", LINE_BUFFSIZE);
		}
		line_index++;
		line = new char[LINE_BUFFSIZE];
	}
	if (feof(file)) {
		printf("End of file reached -> read succesfully!");
	}
	/* may check feof here to make a difference between eof and io failure -- network
	   timeout for instance */

    	fclose(file);
	return lines;

}


void split_by_spaces_convert_to_int(const char *input, int *dest) {
	// copy string
	char str[LINE_BUFFSIZE];
	strcpy(str, input);

	//printf ("Splitting string \"%s\" into tokens:\n",str);
	char * pch;
	// strtok modifies orig string -> unwanted -> copy first
	// 2nd param is delimiters
	pch = strtok (str," \n");
	int i = 0;
	while (pch != NULL)
	{
	  //printf ("'%s'\n",pch);
	  int converted = strtol(pch, NULL, 10);
	  //printf("converted: %d\n", converted);

	  *dest = converted;
	  dest++;
	  pch = strtok (NULL, " ");
	  i++;
	}
}


void print_lines(char **lines) {
	char *current_line = lines[0];
	int i = 1;
	while (current_line != NULL) {
		printf("%s", current_line);
		current_line = lines[i];
		i++;
	}
}



int main() {
	//char **lines = read_text_file_into_buffer("d3_input.txt");
	read_textfile_result input;
	// Relative paths are relative to the current working directory, not the path of the executable.
	// The current working directory is the directory from which you started the program.
	read_text_file_into_buffer("d3_input.txt", &input);
	printf("File has %d lines: really: %d\n", countlines("d3_input.txt"), input.line_count);
	char** lines = input.lines;
	int line_sides[1902][3];

	char *current_line = lines[0];
	int i = 0;
	while (current_line != NULL) {
		// write split line which was converted to 3 ints into int[3] by
		// passing the address
		split_by_spaces_convert_to_int(current_line, &line_sides[i][0]);
		i++;
		current_line = lines[i];
	}

	// check for valid triangles
	unsigned int count = 0;
	unsigned int count2 = 0;
	for (int i=0; i<1902; ++i) {
		if ( (line_sides[i][0] + line_sides[i][1] > line_sides[i][2]) &&
		     (line_sides[i][0] + line_sides[i][2] > line_sides[i][1]) &&
		     (line_sides[i][2] + line_sides[i][1] > line_sides[i][0]) )
			count++;
		
		// since we check 3 rows at once here -> only check every third row
		if (i % 3 == 0) {
			// loop through columns
			for (int j=0; j<3; ++j) {
				if ( (line_sides[i][j] + line_sides[i+1][j] > line_sides[i+2][j]) &&
				     (line_sides[i][j] + line_sides[i+2][j] > line_sides[i+1][j]) &&
				     (line_sides[i+2][j] + line_sides[i+1][j] > line_sides[i][j]) )
					count2++;
			}
		}
	}
	printf("Valid triangles: %d\n", count);
	printf("Valid triangles by column of three: %d", count2);



	return(0);
}
