// C runtime implementation for Pain standard library functions
// This file provides implementations for stdlib functions that are called from LLVM IR

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#define PATH_SEP '\\'
#define PATH_SEP_STR "\\"
#else
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#define PATH_SEP '/'
#define PATH_SEP_STR "/"
#endif

// Simple string list structure for returning lists
typedef struct {
    char** items;
    size_t count;
    size_t capacity;
} StringList;

// File I/O functions
char* read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) {
        return NULL;
    }
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* content = (char*)malloc(size + 1);
    if (!content) {
        fclose(file);
        return NULL;
    }
    
    size_t read = fread(content, 1, size, file);
    content[read] = '\0';
    fclose(file);
    
    return content;
}

void write_file(const char* path, const char* content) {
    FILE* file = fopen(path, "wb");
    if (file) {
        fwrite(content, 1, strlen(content), file);
        fclose(file);
    }
}

// Simple list implementation for read_lines
char** read_lines(const char* path) {
    FILE* file = fopen(path, "r");
    if (!file) {
        return NULL;
    }
    
    StringList list = {0};
    list.capacity = 16;
    list.items = (char**)malloc(list.capacity * sizeof(char*));
    
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), file)) {
        size_t len = strlen(buffer);
        // Remove newline
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
            len--;
        }
        
        if (list.count >= list.capacity) {
            list.capacity *= 2;
            list.items = (char**)realloc(list.items, list.capacity * sizeof(char*));
        }
        
        list.items[list.count] = (char*)malloc(len + 1);
        strcpy(list.items[list.count], buffer);
        list.count++;
    }
    fclose(file);
    
    // Add NULL terminator
    if (list.count >= list.capacity) {
        list.items = (char**)realloc(list.items, (list.capacity + 1) * sizeof(char*));
    }
    list.items[list.count] = NULL;
    
    return list.items;
}

// Path manipulation functions
char* path_join(char** parts) {
    if (!parts || !parts[0]) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    size_t total_len = 0;
    int count = 0;
    while (parts[count]) {
        total_len += strlen(parts[count]);
        count++;
    }
    total_len += count; // For separators
    
    char* result = (char*)malloc(total_len + 1);
    result[0] = '\0';
    
    for (int i = 0; i < count; i++) {
        if (i > 0) {
            strcat(result, PATH_SEP_STR);
        }
        strcat(result, parts[i]);
    }
    
    return result;
}

char* path_dir(const char* path) {
    if (!path) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    const char* last_sep = strrchr(path, PATH_SEP);
    if (!last_sep) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    size_t len = last_sep - path;
    char* result = (char*)malloc(len + 1);
    strncpy(result, path, len);
    result[len] = '\0';
    
    return result;
}

char* path_base(const char* path) {
    if (!path) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    const char* last_sep = strrchr(path, PATH_SEP);
    const char* filename = last_sep ? last_sep + 1 : path;
    
    char* result = (char*)malloc(strlen(filename) + 1);
    strcpy(result, filename);
    
    return result;
}

char* path_ext(const char* path) {
    if (!path) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    const char* last_dot = strrchr(path, '.');
    const char* last_sep = strrchr(path, PATH_SEP);
    
    if (!last_dot || (last_sep && last_dot < last_sep)) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    
    char* result = (char*)malloc(strlen(last_dot) + 1);
    strcpy(result, last_dot);
    
    return result;
}

// Date/time functions
double now(void) {
    return (double)time(NULL);
}

char* time_format(double timestamp, const char* format) {
    time_t t = (time_t)timestamp;
    struct tm* tm_info = localtime(&t);
    
    // Simple format implementation - supports basic formats
    // For full implementation, would need strftime
    char* result = (char*)malloc(256);
    strftime(result, 256, format, tm_info);
    
    return result;
}

// Placeholder implementations for regex and JSON functions
// These would need proper regex and JSON libraries
bool regex_match(const char* pattern, const char* text) {
    // Placeholder - would need regex library
    (void)pattern;
    (void)text;
    return false;
}

char* regex_find(const char* pattern, const char* text) {
    // Placeholder - would need regex library
    (void)pattern;
    (void)text;
    char* empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
}

char** regex_find_all(const char* pattern, const char* text) {
    // Placeholder - would need regex library
    (void)pattern;
    (void)text;
    char** result = (char**)malloc(sizeof(char*));
    result[0] = NULL;
    return result;
}

char* regex_replace(const char* pattern, const char* text, const char* replacement) {
    // Placeholder - would need regex library
    (void)pattern;
    (void)replacement;
    char* result = (char*)malloc(strlen(text) + 1);
    strcpy(result, text);
    return result;
}

char* json_parse(const char* json_str) {
    // Placeholder - would need JSON library
    (void)json_str;
    char* empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
}

char* json_stringify(const char* value) {
    // Placeholder - would need JSON library
    (void)value;
    char* empty = (char*)malloc(1);
    empty[0] = '\0';
    return empty;
}

