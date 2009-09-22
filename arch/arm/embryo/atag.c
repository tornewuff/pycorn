/*
 * ATAG parsing code.
 *
 * This is done very early in the bootstrap process so no mapping can
 * actually happen here; only setting values in bootdata.
 *
 *
 * Copyright 2008 Torne Wuff
 *
 * This file is part of Pycorn.
 *
 * Pycorn is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include "bootstrap.h"
#include "atag.h"

// Parses the atags pointed to by taglist_ptr, if they are valid,
// and stores the various parameters found in bootdata.
// returns 0 if the atags were parsed successfully
int parse_atags()
{
  struct tag *tags = (struct tag *)bootdata->taglist_ptr;
  // The first tag must be an ATAG_CORE - if not it's invalid.
  if (tags->hdr.tag != ATAG_CORE)
    return -1;

  struct tag *t;
  for_each_tag(t, tags)
  {
    DBGINT("tag: ", t->hdr.tag);
    DBGINT("size: ", t->hdr.size);

    switch (t->hdr.tag)
    {
    case ATAG_INITRD2:
      // got the physical addresses of a ramdisk
      if (t->hdr.size >= 4)
      {
        DBGINT("initrd start: ", t->u.initrd.start);
        DBGINT("initrd size: ", t->u.initrd.size);
        bootdata->initrd_phys = t->u.initrd.start;
        bootdata->initrd_size = t->u.initrd.size;
      }
      break;
    }
  }

  return 0;
}
