#include "filesys/filesys.h"
#include <debug.h>
#include <stdio.h>
#include <string.h>
#include "filesys/file.h"
#include "filesys/free-map.h"
#include "filesys/inode.h"
#include "filesys/directory.h"
#include "threads/thread.h"

/* Partition that contains the file system. */
struct block *fs_device;

static void do_format (void);

/* Initializes the file system module.
   If FORMAT is true, reformats the file system. */
void
filesys_init (bool format) 
{
  fs_device = block_get_role (BLOCK_FILESYS);
  if (fs_device == NULL)
    PANIC ("No file system device found, can't initialize file system.");

  inode_init ();
  free_map_init ();

  if (format) 
    do_format ();

  free_map_open ();
}

/* Shuts down the file system module, writing any unwritten data
   to disk. */
void
filesys_done (void) 
{
  free_map_close ();
}

/* Creates a file named NAME with the given INITIAL_SIZE.
   Returns true if successful, false otherwise.
   Fails if a file named NAME already exists,
   or if internal memory allocation fails. */
bool
filesys_create (const char *name, off_t initial_size, FILE_TYPE type) 
{
  block_sector_t inode_sector = 0;
  struct dir *dir;
  char file_name[NAME_MAX+1];
  struct inode * inode;
  bool success;

  /* Getting innermost dir and parsed file name. */
  if(dir_get_dir_and_file(name, &dir , file_name) == false){
    return false;
  }

  /* Fail if file with same name already exists. */
  if(dir_lookup(dir, file_name, &inode) == true){
    inode_close(inode);//
    dir_close(dir);
    return false;
  }

  block_sector_t parent_dir = inode_get_inumber(dir_get_inode(dir));
  if(type == FILE){
    success = (dir!=NULL
                    && free_map_allocate(1, &inode_sector)
                    && inode_create(inode_sector, initial_size, type, parent_dir)
                    && dir_add(dir, file_name, inode_sector));
                    //allocate space for inode on disk.
                    //inode_create creates inode & data blocks.
                    //
  }else{
    success = (dir!=NULL
                    && free_map_allocate(1, &inode_sector)
                    && dir_create(inode_sector, 16, parent_dir)
                    && dir_add(dir, file_name, inode_sector));
                    //allocate space for inode on disk.
                    //inode_create creates inode & data blocks.
                    //

  }

  if (!success && inode_sector != 0) 
    free_map_release (inode_sector, 1);

  // if(success){
  //   ASSERT(dir_lookup(dir, file_name, &inode) == true);
  // }

  dir_close (dir);

  return success;
}

/* Opens the file with the given NAME.
   Returns the new file if successful or a null pointer
   otherwise.
   Fails if no file named NAME exists,
   or if an internal memory allocation fails. */
struct file *
filesys_open (const char *name)
{
  struct inode *inode = NULL;
  struct dir * dir;
  char file_name[NAME_MAX+1];
  struct file * file;

  /* Getting innermost dir and parsed file name. */
  if(dir_get_dir_and_file(name, &dir , file_name) == false){
    return false;
  }

  /* Special Cases */
  if(strcmp(name, "/") == 0){
    file = file_open(inode_reopen(dir_get_inode(dir)));
    dir_close(dir);
    return file;
  }
  if(strcmp(file_name, ".") == 0){
    file = file_open(inode_reopen(dir_get_inode(dir)));
    dir_close(dir);
    return file;
  }

  ASSERT(dir != NULL);
  dir_lookup(dir, file_name, &inode);

  return file_open (inode);
}

/* Deletes the file named NAME.
   Returns true if successful, false on failure.
   Fails if no file named NAME exists,
   or if an internal memory allocation fails. */
bool
filesys_remove (const char *name) 
{
  struct dir * dir;
  char file_name[NAME_MAX+1];
  struct inode * inode;
  char n[NAME_MAX + 1];

  /* If incorrect name given, fail. */
  if(dir_get_dir_and_file(name, &dir, file_name) == false){
    return false;
  }


  dir_lookup(dir, file_name,&inode);

  /* If file doesn't exist.*/
  if(inode == NULL){
    // printf("fail1\n");
    return false;
  }

  /* Don't allow removing current directory, or open file. */
  if(dir_get_inode(thread_current()->cur_dir) == inode || inode_get_open_cnt(inode) > 2){
    dir_close(dir); //techncically should be 1.
    // printf("fail2%d\n",inode_get_open_cnt(inode));
    return false;
  } 
  /* Don't allow deletion if directory not empty. */
  if(inode_get_file_type(inode)==DIR){
    struct dir * temp = dir_open(inode);
    if(dir_readdir(temp, n) == true){//if directory not empty.
      // printf("fail3\n");
      dir_close(temp);
      dir_close(dir);
      return false;
    }
    dir_close(temp);
  }

  bool success = dir != NULL && dir_remove (dir, file_name);
  dir_close (dir); 

  return success;
}

/* Formats the file system. */
static void
do_format (void)
{
  printf ("Formatting file system...");
  free_map_create ();
  if (!dir_create (ROOT_DIR_SECTOR, 16, -1))
    PANIC ("root directory creation failed");
  free_map_close ();
  printf ("done.\n");
}
