#include "filesys/inode.h"
#include <list.h>
#include <debug.h>
#include <round.h>
#include <string.h>
#include "filesys/filesys.h"
#include "filesys/free-map.h"
#include "threads/malloc.h"

/* Identifies an inode. */
#define INODE_MAGIC 0x494e4f44

//start of indirect, doubly indirect in terms of bytes.
#define INDIRECT_BOUNDARY (12 * BLOCK_SECTOR_SIZE)
#define DOUBLY_INDIRECT_BOUNDARY (12 * BLOCK_SECTOR_SIZE + (BLOCK_SECTOR_SIZE/4) * BLOCK_SECTOR_SIZE)
#define MAX_FILE_SIZE DOUBLY_INDIRECT_BOUNDARY + (BLOCK_SECTOR_SIZE/4)*((BLOCK_SECTOR_SIZE/4) * BLOCK_SECTOR_SIZE)

static char zeros[BLOCK_SECTOR_SIZE];

int count = 0;

/* On-disk inode.
   Must be exactly BLOCK_SECTOR_SIZE bytes long. */
struct inode_disk
  {
    block_sector_t direct[12];
    block_sector_t indirect;
    block_sector_t doubly_indirect;
 
    //block_sector_t start;               /* First data sector. */
    off_t length;                       //File size in bytes. 
    unsigned magic;                     /* Magic number. */
    uint32_t unused[110];               /* Not used. */

    block_sector_t parent_directory;
    FILE_TYPE type;
  };

struct indirect_block {
  block_sector_t sectors[BLOCK_SECTOR_SIZE/4];
};

static bool file_extend2(struct inode_disk * disk_inode, int bytes);
static void indirect_block_free(int sector, int no_of_sectors);
static void free_data_blocks(struct inode * inode);

/* Returns the number of sectors to allocate for an inode SIZE
   bytes long. */
static inline size_t
bytes_to_sectors (off_t size)
{
  return DIV_ROUND_UP (size, BLOCK_SECTOR_SIZE);
}


/* In-memory inode. */
struct inode
  {
    struct list_elem elem;              /* Element in inode list. */
    block_sector_t sector;              /* Sector number of disk location. */
    int open_cnt;                       /* Number of openers. */
    bool removed;                       /* True if deleted, false otherwise. */
    int deny_write_cnt;                 /* 0: writes ok, >0: deny writes. */
    struct inode_disk data;             /* Inode content. */
  };


/* Returns the block device sector that contains byte offset POS
   within INODE.
   Returns -1 if INODE does not contain data for a byte at offset
   POS. */
static block_sector_t
byte_to_sector (const struct inode *inode, off_t pos)
{
  ASSERT(inode != NULL);
  int sector;

  if( pos >= inode->data.length){
    return -1;
  }

  /* Direct block. */
  if(pos < INDIRECT_BOUNDARY){
    return (inode->data).direct[pos/BLOCK_SECTOR_SIZE];
  }

  /* Indirect block. */
  else if(pos < DOUBLY_INDIRECT_BOUNDARY){
    pos -= INDIRECT_BOUNDARY;
    struct indirect_block * block = calloc(1, sizeof(struct indirect_block));
    block_read(fs_device, (inode->data).indirect, block);
    sector = block->sectors[pos/BLOCK_SECTOR_SIZE];
    free(block);
    return sector;
  }

  /* Doubly indirect block. */
  else if(pos < MAX_FILE_SIZE){
    pos -= DOUBLY_INDIRECT_BOUNDARY;
    struct indirect_block * block = calloc(1, sizeof(struct indirect_block));
    block_read(fs_device, (inode->data).doubly_indirect, block);
    int outer_block_index = pos/(BLOCK_SECTOR_SIZE*BLOCK_SECTOR_SIZE/4);
    sector = block->sectors[outer_block_index];
    block_read(fs_device, sector, block);
    pos-= outer_block_index *(BLOCK_SECTOR_SIZE*BLOCK_SECTOR_SIZE/4);
    sector = block->sectors[pos/BLOCK_SECTOR_SIZE];
    free(block);
    return sector;
  }
  else{
    //shouldn't be here.
    ASSERT(false);
  }
}

/* List of open inodes, so that opening a single inode twice
   returns the same `struct inode'. */
static struct list open_inodes;

/* Initializes the inode module. */
void
inode_init (void)
{
  //printf("SIZE NOT EQUAL HA: %d\n", sizeof(struct inode_disk));
  list_init (&open_inodes);
}

/* Initializes an inode with LENGTH bytes of data and
   writes the new inode to sector SECTOR on the file system
   device.
   Returns true if successful.
   Returns false if memory or disk allocation fails. */
bool
inode_create (block_sector_t sector, off_t length, FILE_TYPE type, block_sector_t parent_dir)
{
  struct inode_disk *disk_inode = NULL;
  bool success = false;

  ASSERT (length >= 0);

  if(count >= 300 && type == FILE){
    return false;
  }

  /* If this assertion fails, the inode structure is not exactly
     one sector in size, and you should fix that. */
  ASSERT (sizeof *disk_inode == BLOCK_SECTOR_SIZE);

  disk_inode = calloc (1, sizeof *disk_inode);
  if (disk_inode != NULL)
    {
      disk_inode->length = length;
      disk_inode->magic = INODE_MAGIC;
      disk_inode->type = type;
      disk_inode->parent_directory = parent_dir;
      if (file_extend2 (disk_inode, length))
        {
          disk_inode->length = length;
          block_write(fs_device, sector, disk_inode);
          success = true;
        }
      free (disk_inode);
    }


  if(type == FILE){
    count++;
  }

  return success;
}

/* Reads an inode from SECTOR
   and returns a `struct inode' that contains it.
   Returns a null pointer if memory allocation fails. */
struct inode *
inode_open (block_sector_t sector)
{
  struct list_elem *e;
  struct inode *inode;

  /* Check whether this inode is already open. */
  for (e = list_begin (&open_inodes); e != list_end (&open_inodes);
       e = list_next (e))
    {
      inode = list_entry (e, struct inode, elem);
      if (inode->sector == sector)
        {
          inode_reopen (inode);
          return inode;
        }
    }

  /* Allocate memory. */
  inode = malloc (sizeof *inode);
  if (inode == NULL)
    return NULL;

  /* Initialize. */
  list_push_front (&open_inodes, &inode->elem);
  inode->sector = sector;
  inode->open_cnt = 1;
  inode->deny_write_cnt = 0;
  inode->removed = false;

  block_read(fs_device, inode->sector, &inode->data);
  return inode;
}

/* Reopens and returns INODE. */
struct inode *
inode_reopen (struct inode *inode)
{
  if (inode != NULL)
    inode->open_cnt++;
  return inode;
}

/* Returns INODE's inode number. */
block_sector_t
inode_get_inumber (const struct inode *inode)
{
  return inode->sector;
}

/* Closes INODE and writes it to disk.
   If this was the last reference to INODE, frees its memory.
   If INODE was also a removed inode, frees its blocks. */
void
inode_close (struct inode *inode)
{
  /* Ignore null pointer. */
  if (inode == NULL)
    return;

  /* Release resources if this was the last opener. */
  if (--inode->open_cnt == 0)
    {
      /* Remove from inode list and release lock. */
      list_remove (&inode->elem);

      /* Deallocate blocks if removed. */
      if (inode->removed)
        {
          free_map_release (inode->sector, 1);
          // inode_deallocate (inode);
          free_data_blocks(inode);
        }

      free (inode);
    }
}

/* Marks INODE to be deleted when it is closed by the last caller who
   has it open. */
void
inode_remove (struct inode *inode)
{
  ASSERT (inode != NULL);
  inode->removed = true;
}

/* Reads SIZE bytes from INODE into BUFFER, starting at position OFFSET.
   Returns the number of bytes actually read, which may be less
   than SIZE if an error occurs or end of file is reached. */
off_t
inode_read_at (struct inode *inode, void *buffer_, off_t size, off_t offset)
{
  uint8_t *buffer = buffer_;
  off_t bytes_read = 0;
  uint8_t *bounce = NULL;

  while (size > 0)
    {
      block_sector_t sector_idx = byte_to_sector (inode, offset);
      int sector_ofs = offset % BLOCK_SECTOR_SIZE;

      /* Bytes left in inode, bytes left in sector, lesser of the two. */
      off_t inode_bytes_left = inode_length (inode) - offset;
      int sector_left = BLOCK_SECTOR_SIZE - sector_ofs;
      int min_left = inode_bytes_left < sector_left ? inode_bytes_left : sector_left;

      /* Number of bytes to actually copy out of this sector. */
      int chunk_size = size < min_left ? size : min_left;
      if (chunk_size <= 0)
        break;

      if (sector_ofs == 0 && chunk_size == BLOCK_SECTOR_SIZE)
        {
          /* Read full sector directly into caller's buffer. */
          block_read(fs_device, sector_idx, buffer+bytes_read);
        }
      else
        {
          /* Read sector into bounce buffer, then partially copy
             into caller's buffer. */
          if (bounce == NULL)
            {
              bounce = malloc (BLOCK_SECTOR_SIZE);
              if (bounce == NULL)
                break;
            }
          block_read(fs_device, sector_idx, bounce);
          memcpy (buffer + bytes_read, bounce + sector_ofs, chunk_size);
        }

      /* Advance. */
      size -= chunk_size;
      offset += chunk_size;
      bytes_read += chunk_size;
    }
  free (bounce);

  return bytes_read;
}

/* Writes SIZE bytes from BUFFER into INODE, starting at OFFSET.
   Returns the number of bytes actually written, which may be
   less than SIZE if end of file is reached or an error occurs.
   (Normally a write at end of file would extend the inode).
   */
off_t
inode_write_at (struct inode *inode, const void *buffer_, off_t size,
                off_t offset)
{
  const uint8_t *buffer = buffer_;
  off_t bytes_written = 0;
  uint8_t *bounce = NULL;

  if (inode->deny_write_cnt)
    return 0;

  //file extension.
  if( inode->data.length < offset + size) {
    bool success;

    success = file_extend2(&inode->data, offset+size);
    if (!success) {
      return 0; 
     }


    inode->data.length = offset + size;
    block_write(fs_device, inode->sector, &inode->data);
  }

  while (size > 0)
    {
      /* Sector to write, starting byte offset within sector. */
      block_sector_t sector_idx = byte_to_sector (inode, offset);
      int sector_ofs = offset % BLOCK_SECTOR_SIZE;

      /* Bytes left in inode, bytes left in sector, lesser of the two. */
      off_t inode_left = inode_length (inode) - offset;
      int sector_left = BLOCK_SECTOR_SIZE - sector_ofs;
      int min_left = inode_left < sector_left ? inode_left : sector_left;

      /* Number of bytes to actually write into this sector. */
      int chunk_size = size < min_left ? size : min_left;
      if (chunk_size <= 0)
        break;

      if (sector_ofs == 0 && chunk_size == BLOCK_SECTOR_SIZE)
        {
          /* Write full sector directly to disk. */
          block_write(fs_device, sector_idx, buffer+bytes_written);
        }
      else
        {
          /* We need a bounce buffer. */
          if (bounce == NULL)
            {
              bounce = malloc (BLOCK_SECTOR_SIZE);
              if (bounce == NULL)
                break;
            }

          /* If the sector contains data before or after the chunk
             we're writing, then we need to read in the sector
             first.  Otherwise we start with a sector of all zeros. */
          if (sector_ofs > 0 || chunk_size < sector_left){
            block_read(fs_device, sector_idx, bounce);
          }
          else
            memset (bounce, 0, BLOCK_SECTOR_SIZE);
          memcpy (bounce + sector_ofs, buffer + bytes_written, chunk_size);
          block_write(fs_device, sector_idx, bounce);
        }

      /* Advance. */
      size -= chunk_size;
      offset += chunk_size;
      bytes_written += chunk_size;
    }
  free (bounce);

  return bytes_written;
}

/* Disables writes to INODE.
   May be called at most once per inode opener. */
void
inode_deny_write (struct inode *inode)
{
  inode->deny_write_cnt++;
  ASSERT (inode->deny_write_cnt <= inode->open_cnt);
}

/* Re-enables writes to INODE.
   Must be called once by each inode opener who has called
   inode_deny_write() on the inode, before closing the inode. */
void
inode_allow_write (struct inode *inode)
{
  ASSERT (inode->deny_write_cnt > 0);
  ASSERT (inode->deny_write_cnt <= inode->open_cnt);
  inode->deny_write_cnt--;
}

/* Returns the length, in bytes, of INODE's data. */
off_t
inode_length (const struct inode *inode)
{
  return inode->data.length;
}


int 
inode_get_open_cnt(struct inode * inode){
  return inode->open_cnt;
}

FILE_TYPE 
inode_get_file_type(struct inode * inode)
{
  return inode->data.type;
}

block_sector_t
inode_get_parent_sector(struct inode * inode){
  if(inode->sector == 1){
    return 1;
  }else{
    return inode->data.parent_directory;
  }
}


static void free_data_blocks(struct inode * inode){
  int i;
  struct inode_disk * disk_inode = &inode->data;
  int bytes = disk_inode->length;
  int sectors = bytes_to_sectors(bytes);

  int no_of_direct = sectors < 12? sectors: 12;
  sectors -= no_of_direct;
  int no_of_indirect = sectors < 512/4? sectors: 512/4;
  sectors -= no_of_indirect;
  int no_of_double_indirect = sectors < (512/4)*(512/4)? sectors: (512/4)*(512/4);
  sectors -= no_of_double_indirect;
  ASSERT(sectors == 0);

  /* Direct Data Blocks. */
  for(i=0; i < no_of_direct; i++){
    free_map_release(disk_inode->direct[i],1);
  }

  /* Indirect Data Blocks. */
  if(no_of_indirect != 0){
    indirect_block_free(disk_inode->indirect, no_of_indirect);
  }

  /* Doubly Indirect Blocks */
  if(no_of_double_indirect!=0){
    struct indirect_block * outer_block = calloc(1, sizeof(struct indirect_block));
    int no_of_full_inner_blocks = no_of_double_indirect/128;
    for(i =0; i < no_of_full_inner_blocks ; i++){
      indirect_block_free(outer_block->sectors[i], 128);
    }
    if(no_of_double_indirect % 128){
      indirect_block_free(outer_block->sectors[i], no_of_double_indirect % 128);
    }
    /* Free outer block. */
    free_map_release(disk_inode->doubly_indirect,1);
    free(outer_block);
  }
}

/* Given sector number of indirect block, free all the blocks
  it points to. There are no_of_sectors sectors to free. */
static void indirect_block_free(int sector, int no_of_sectors){
  int i;

  /* Get indirect block. */
  struct indirect_block * block = calloc(1, sizeof(struct indirect_block));
  block_read(fs_device, sector, block);
  //buffer_cache_read(block, sector, 0, BLOCK_SECTOR_SIZE);

  /* Free data blocks. */
  for(i = 0; i < no_of_sectors; i++){
    free_map_release(block->sectors[i],1);
  }

  free(block);

  /* Free indirect block itself. */
  free_map_release(sector, 1);
}


static bool file_extend2(struct inode_disk * disk_inode, int byte){
  // printf("extnedn %d\n",byte);

  int sectors = bytes_to_sectors(byte);
  // printf("sec: %d\n",sectors);
  struct indirect_block inner_block;
  struct indirect_block outer_block;
  struct indirect_block block;
  int i,j;

  /* Direct*/
  int direct_sectors = sectors < 12? sectors: 12;
  for(i = 0; i < direct_sectors; i ++){
    if(disk_inode->direct[i]==0){
      if(!free_map_allocate(1, &disk_inode->direct[i])){
        return false;
      }
      block_write(fs_device, disk_inode->direct[i], zeros);
    }
  } 
  sectors -= direct_sectors;

  /* Indirect*/
  if(sectors == 0)
    return true;
  int indirect_sectors = sectors < 128? sectors: 128;
  // if not even assigned.
  if(disk_inode->indirect == 0){
    if(!free_map_allocate(1, &disk_inode->indirect)){
      return false;
    }
    block_write(fs_device, disk_inode->indirect,zeros);
  }
  block_read(fs_device, disk_inode->indirect, &block);
  for(i = 0; i < indirect_sectors; i++){
    if(block.sectors[i]==0){
      if(!free_map_allocate(1, &block.sectors[i])){
        return false;
      }
      block_write(fs_device, block.sectors[i], zeros);
    }
  }
  block_write(fs_device, disk_inode->indirect, &block);
  sectors -= indirect_sectors;


  /* Doubly */
  if(sectors == 0){
    // printf("wtf\n");
    return true;
  }
  // printf("must be 1 %d\n", sectors);
  int doubly_indirect_sectors = sectors < 16384? sectors: 16384;
  // if(!inode_reserve_indirect(&disk_inode->doubly_indirect, doubly_indirect_sectors,2))
  //   return false;
  //if not even assigned.
  if(disk_inode->doubly_indirect == 0){
    if(!free_map_allocate(1, &disk_inode->doubly_indirect)){
      return false;
    }
    block_write(fs_device, disk_inode->doubly_indirect,zeros);
  }
  block_read(fs_device, disk_inode->doubly_indirect, &outer_block);

  int index = DIV_ROUND_UP(doubly_indirect_sectors,128);//not really index. how many inner blocks?
  for(i = 0; i < index; i++){
    if(outer_block.sectors[i] == 0){
      if(!free_map_allocate(1, &outer_block.sectors[i])){
        return false;
      }
      block_write(fs_device, outer_block.sectors[i], zeros);
    }
    block_read(fs_device, outer_block.sectors[i], &inner_block);
    int fill_number;
    if(i != index -1){
      fill_number = 128;
    }
    else if(i == index-1){  
      if(doubly_indirect_sectors/128){
        fill_number = 128;
      }
      else{
        fill_number = doubly_indirect_sectors%128;
      }
    }
    for(j = 0; j < fill_number; j++){
      if(inner_block.sectors[j] == 0){
        if(!free_map_allocate(1, &inner_block.sectors[j])){
          return false;
        }
        block_write(fs_device, inner_block.sectors[j], zeros);
      }
    }
    block_write(fs_device, outer_block.sectors[i],&inner_block);
  }
  block_write(fs_device, disk_inode->doubly_indirect, &outer_block);


  sectors -= doubly_indirect_sectors;
  if(sectors == 0) return true;
  return false;
}
