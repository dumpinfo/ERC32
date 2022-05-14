/*
 * This file is part of MKPROM.
 * 
 * MKPROM, ERC32 boot-prom utility. Copyright (C) 1995 Jiri Gaisler, European
 * Space Agency
 * 
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 675
 * Mass Ave, Cambridge, MA 02139, USA.
 * 
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef fprintf
extern          fprintf();
#endif

#define	VAL(x)	strtol(x,(char **)NULL,0)
#define	SECMAX	32
#define	SECNAME	16

typedef struct sectype {
    unsigned int    paddr;
    unsigned int    raddr;
    unsigned int    len;
    unsigned int    comp;
    unsigned char   name[SECNAME];
}               tt;

struct sectype  secarr[SECMAX];
char            filename[128];

const char      version[] = "v1.2.1";
int             secnum = 0;
FILE           *dumpfile;

int             verbose = 0;

main(argc, argv)
    int             argc;
    char          **argv;

{

    char	    cmd[512];
    int             baud = 19200;	/* 19200 baud */
    int             edac = 1;
    int             par = 1;
    double          freq = 1E7;
    double          wdfreq = 0.0;
    int             i;
    int             mctrl = 0x0080001f;
    int             memcfg = 0;
    int             prot = 1;
    int             ramcs = 0;
    int             ramsize = 0x200000;	/* 2M ram */
    int             romsize = 0x80000;	/* 512K rom */
    int             ramws = 0;
    int             romws = 2;
    int             stack = 0;
    int             stat = 1;
    int             waitcfg = 0;
    int             wdog = 1;
    int             comp = 1;
    int             dump = 0;
    char	    ofile[128] = "prom.out";


    printf("\nMKPROM - ERC32 boot-prom builder %s,  copyright Jiri Gaisler, ESA/ESTEC 1997\n", version);
    printf("Bug-reports to jgais@ws.estec.esa.nl\n\n");
    if ((dumpfile = fopen("dump.s", "w+")) == NULL) {
	printf("Failed to open temporary file\n");
	exit(1);
    }
    while (stat < argc) {
	if (argv[stat][0] == '-') {
	    if (strcmp(argv[stat], "-v") == 0) {
		verbose = 1;
	    } else if (strcmp(argv[stat], "-baud") == 0) {
		if ((stat + 1) < argc)
		    baud = VAL(argv[++stat]);
	    } else if (strcmp(argv[stat], "-wdog") == 0) {
		wdog = 0;
	    } else if (strcmp(argv[stat], "-dump") == 0) {
		dump = 1;
	    } else if (strcmp(argv[stat], "-nocomp") == 0) {
		comp = 0;
	    } else if (strcmp(argv[stat], "-nopar") == 0) {
		par = 0;
	    } else if (strcmp(argv[stat], "-noedac") == 0) {
		edac = 0;
	    } else if (strcmp(argv[stat], "-freq") == 0) {
		if ((stat + 1) < argc)
		    freq = atof(argv[++stat]);
		    freq *= 1E6;
	    } else if (strcmp(argv[stat], "-wdfreq") == 0) {
		if ((stat + 1) < argc)
		    wdfreq = atof(argv[++stat]);
		    wdfreq *= 1E6;
	    } else if (strcmp(argv[stat], "-noprot") == 0) {
		prot = 0; mctrl &= ~0x8;
	    } else if (strcmp(argv[stat], "-o") == 0) {
		strncpy(ofile,argv[++stat],127);
		ofile[127] = 0;
	    } else if (strcmp(argv[stat], "-ramsize") == 0) {
		if ((stat + 1) < argc) {
		    ramsize = (VAL(argv[++stat])) & 0x03ffff;
		    ramsize *= 1024;
		}
	    } else if (strcmp(argv[stat], "-romws") == 0) {
		if ((stat + 1) < argc)
		    romws = (VAL(argv[++stat])) & 0xf;
	    } else if (strcmp(argv[stat], "-romsize") == 0) {
		if ((stat + 1) < argc) {
		    romsize = (VAL(argv[++stat])) & 0x01ffff;
		    romsize *= 1024;
		}
	    } else if (strcmp(argv[stat], "-ramcs") == 0) {
		if ((stat + 1) < argc)
		    ramcs = (VAL(argv[++stat])) & 0x0f;
		switch (ramcs) {
		case 1:
		    ramcs = 0;
		    break;
		case 2:
		    ramcs = 1;
		    break;
		case 4:
		    ramcs = 2;
		    break;
		case 8:
		    ramcs = 3;
		    break;
		default:
		    ramcs = 0;
		}
	    } else if (strcmp(argv[stat], "-stack") == 0) {
		if ((stat + 1) < argc)
		    stack = (VAL(argv[++stat])) & ~0x01f;
	    } else if (strcmp(argv[stat], "-ramws") == 0) {
		if ((stat + 1) < argc)
		    ramws = (VAL(argv[++stat])) & 0x3;
	    } else {
		printf("unknown option %s\n", argv[stat]);
		usage();
		exit(1);
	    }
	} else {
	    if (secnum == 0)
		strcpy(filename, argv[stat]);
	    bfd_load(argv[stat], comp);
	}
	stat++;
    }
    fprintf(dumpfile, "\n\t.global _filename\n");
    fprintf(dumpfile, "_filename:\n");
    fprintf(dumpfile, "\t.string\t\"%s\"\n", filename);
    fprintf(dumpfile, "\n\t.align 32\n");
    fprintf(dumpfile, "\t.global _sections\n");
    fprintf(dumpfile, "_sections:\n");
    for (i = 0; i < secnum; i++) {
	fprintf(dumpfile, "\t.word\t0x%x\n", secarr[i].paddr);
	fprintf(dumpfile, "\t.word\t_section%d\n", i);
	fprintf(dumpfile, "\t.word\t0x%x\n", secarr[i].len);
	fprintf(dumpfile, "\t.word\t0x%x\n", secarr[i].comp);
	fprintf(dumpfile, "\t.string\t\"%s\"\n", secarr[i].name);
	fprintf(dumpfile, "\n\t.align 32\n");
    }
    fprintf(dumpfile, "\t.word\t0\n");

    fprintf(dumpfile, "\n\t.global _wdog, _mctrl, _ramsize, _stack\n");
    fprintf(dumpfile, "\t.global _waitcfg, _memcfg, _prot, _freq\n");
    fprintf(dumpfile, "_freq:\n");
    fprintf(dumpfile, "\t.word\t%d\n", (int) (freq/1000000));
    fprintf(dumpfile, "_wdog:\n");
    fprintf(dumpfile, "\t.word\t%d\n", wdog);
    fprintf(dumpfile, "_mctrl:\n");
    if (wdfreq > 0.0) {
        mctrl |= (((int)wdfreq / (64 * baud) - 1) << 24);
	mctrl &= 0xff7fffff;
    } else
        mctrl |= (((int)freq / (64 * baud) - 1) << 24);
    fprintf(dumpfile, "\t.word\t0x%x\n", mctrl);
    fprintf(dumpfile, "_waitcfg:\n");
    if (romws)
	romws++;
    waitcfg |= romws << 4;
    waitcfg |= (0x3 & ramws) << 2;
    fprintf(dumpfile, "\t.word\t0x%x\n", waitcfg);
    fprintf(dumpfile, "_prot:\n");
    fprintf(dumpfile, "\t.word\t%d\n", prot);
    fprintf(dumpfile, "_ramsize:\n");
    fprintf(dumpfile, "\t.word\t0x%x\n", ramsize);
    if (!stack)
	stack = 0x2000000 + ramsize - 32;
    fprintf(dumpfile, "_stack:\n");
    fprintf(dumpfile, "\t.word\t0x%x\n", stack);
    fprintf(dumpfile, "_memcfg:\n");
    memcfg |= (memcfg & ~3) | ramcs;
    i = 0;
    ramsize >>= 18;
    while (ramsize) {
	i++;
	ramsize >>= 1;
    }
    if (i)
	i--;
    memcfg |= (memcfg & ~0x1c00) | (i << 10);
    i = 0;
    romsize >>= 17;
    while (romsize) {
	i++;
	romsize >>= 1;
    }
    if (i)
	i--;
    memcfg |= (memcfg & ~0x1c0000) | (i << 18);
    memcfg |= (edac << 14);
    memcfg |= (par << 13);
    fprintf(dumpfile, "\t.word\t0x%x\n", memcfg);
    fclose(dumpfile);

    strcpy(cmd,"sparc-rtems-gcc -O2 -T linkboot -nostartfiles \
/usr/local/erc32/sparc-rtems/lib/promcore.o \
/usr/local/erc32/sparc-rtems/lib/promload.o \
/usr/local/erc32/sparc-rtems/lib/decomp.o \
-nostdlib dump.s -o ");
    strcat(cmd,ofile);
    if (verbose) printf("\n%s\n", cmd);
    system(cmd);
    if (!dump) system("rm -f dump.s");
    exit(0);
}


usage()
{
}


#define N   4096		/* size of ring buffer (maximum is 4096
				 * because the pointer is coded in 12 bits */
#define F   18			/* upper limit for match_length (maximum is
				 * 15 + THRESHOLD + 1 because the length is
				 * coded in 4 bits) */

#define THRESHOLD  2		/* encode string into position and length if
				 * match_length is greater than this
				 * (compression) */
#define NIL  N			/* index for root of binary search trees */
#define MAGIC_NUMBER '\xaa'
#define EOP '\x55'
#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef SEEK_END
#define SEEK_END 2
#endif

unsigned char
                text_buf[N + F - 1];	/* ring buffer of size N, with extra
					 * F-1 bytes to facilitate string
					 * comparison */

int
                match_position, match_length,	/* of longest match.  These
						 * are set by the
						 * InsertNode() procedure. */
                lson[N + 1], rson[N + 257], dad[N + 1];	/* left & right children
							 * & parents -- These
							 * constitute binary
							 * search trees. */
unsigned long
                textsize = 0,	/* text size counter */
                codesize = 0,	/* code size counter */
                printcount = 0;	/* counter for reporting progress every 1K
				 * bytes */


unsigned char   CHECKSUM;

typedef struct {
    char            MAGIC;
    unsigned char   PARAMS;
    unsigned char   CHECKSUM;
    unsigned char   dummy;
    unsigned char   ENCODED_SIZE[4];
    unsigned char   DECODED_SIZE[4];
}               packet_header;

#define PH_SIZE 12

int
PutPacketInfo(buf)
    char           *buf;
{
    packet_header   PH;

    PH.MAGIC = MAGIC_NUMBER;
    PH.PARAMS = (unsigned char) (((N >> 6) & 0xf0) |
			  ((((F / 18) % 3) << 2) & 0x0c) | (THRESHOLD - 1));
    PH.CHECKSUM = CHECKSUM;
    PH.ENCODED_SIZE[0] = (codesize >> 24);
    PH.ENCODED_SIZE[1] = (codesize >> 16);
    PH.ENCODED_SIZE[2] = (codesize >> 8);
    PH.ENCODED_SIZE[3] = codesize;
    PH.DECODED_SIZE[0] = textsize >> 24;
    PH.DECODED_SIZE[1] = textsize >> 16;
    PH.DECODED_SIZE[2] = textsize >> 8;
    PH.DECODED_SIZE[3] = textsize;
    memcpy(buf, &PH, sizeof(packet_header));
/*
    printf("Packet info:\n" "params: %x\n" "checksum: %x\n", PH.PARAMS, PH.CHECKSUM);
*/
    return 0;
}

void
InitTree(void)
{				/* initialize trees */
    int             i;

    /*
     * For i = 0 to N - 1, rson[i] and lson[i] will be the right and left
     * children of node i.  These nodes need not be initialized. Also, dad[i]
     * is the parent of node i.  These are initialized to NIL (= N), which
     * stands for 'not used.' For i = 0 to 255, rson[N + i + 1] is the root
     * of the tree for strings that begin with character i.  These are
     * initialized to NIL.  Note there are 256 trees.
     */

    for (i = N + 1; i <= N + 256; i++)
	rson[i] = NIL;
    for (i = 0; i < N; i++)
	dad[i] = NIL;
}

void
InsertNode(int r)
/*
 * Inserts string of length F, text_buf[r..r+F-1], into one of the trees
 * (text_buf[r]'th tree) and returns the longest-match position and length
 * via the global variables match_position and match_length. If match_length
 * = F, then removes the old node in favor of the new one, because the old
 * one will be deleted sooner. Note r plays double role, as tree node and
 * position in buffer.
 */
{
    int             i, p, cmp;
    unsigned char  *key;

    cmp = 1;
    key = &text_buf[r];
    p = N + 1 + key[0];
    rson[r] = lson[r] = NIL;
    match_length = 0;
    for (;;) {
	if (cmp >= 0) {
	    if (rson[p] != NIL)
		p = rson[p];
	    else {
		rson[p] = r;
		dad[r] = p;
		return;
	    }
	} else {
	    if (lson[p] != NIL)
		p = lson[p];
	    else {
		lson[p] = r;
		dad[r] = p;
		return;
	    }
	}
	for (i = 1; i < F; i++)
	    if ((cmp = key[i] - text_buf[p + i]) != 0)
		break;
	if (i > match_length) {
	    match_position = p;
	    if ((match_length = i) >= F)
		break;
	}
    }
    dad[r] = dad[p];
    lson[r] = lson[p];
    rson[r] = rson[p];
    dad[lson[p]] = r;
    dad[rson[p]] = r;
    if (rson[dad[p]] == p)
	rson[dad[p]] = r;
    else
	lson[dad[p]] = r;
    dad[p] = NIL;		/* remove p */
}

void
DeleteNode(int p)
{				/* deletes node p from tree */
    int             q;

    if (dad[p] == NIL)
	return;			/* not in tree */
    if (rson[p] == NIL)
	q = lson[p];
    else if (lson[p] == NIL)
	q = rson[p];
    else {
	q = lson[p];
	if (rson[q] != NIL) {
	    do {
		q = rson[q];
	    } while (rson[q] != NIL);
	    rson[dad[q]] = lson[q];
	    dad[lson[q]] = dad[q];
	    lson[q] = lson[p];
	    dad[lson[p]] = q;
	}
	rson[q] = rson[p];
	dad[rson[p]] = q;
    }
    dad[q] = dad[p];
    if (rson[dad[p]] == p)
	rson[dad[p]] = q;
    else
	lson[dad[p]] = q;
    dad[p] = NIL;
}

int
Encode(inbuf, outbuf, buflen, oindex)
    unsigned char           *inbuf;
    unsigned char           *outbuf;
    int             buflen, oindex;
{
    int             i, c, len, r, s, last_match_length, code_buf_ptr;
    unsigned char   code_buf[17], mask;

    int             lindex = 0;

    CHECKSUM = 0xff;
    InitTree();			/* initialize trees */
    code_buf[0] = 0;		/* code_buf[1..16] saves eight units of code,
				 * and code_buf[0] works as eight flags, "1"
				 * representing that the unit is an unencoded
				 * letter (1 byte), "0" a position-and-length
				 * pair (2 bytes).  Thus, eight units require
				 * at most 16 bytes of code. */
    code_buf_ptr = mask = 1;
    s = 0;
    r = N - F;
    for (i = s; i < r; i++)
	text_buf[i] = ' ';	/* Clear the buffer with any character that
				 * will appear often. */
    for (len = 0; len < F && (lindex < buflen); len++) {
	c = inbuf[lindex++];
	CHECKSUM ^= c;
	text_buf[r + len] = c;	/* Read F bytes into the last F bytes of the
				 * buffer */
    }
    if ((textsize = len) == 0)
	return;			/* text of size zero */
    for (i = 1; i <= F; i++)
	InsertNode(r - i);	/* Insert the F strings, each of which begins
				 * with one or more 'space' characters.  Note
				 * the order in which these strings are
				 * inserted.  This way, degenerate trees will
				 * be less likely to occur. */
    InsertNode(r);		/* Finally, insert the whole string just
				 * read.  The global variables match_length
				 * and match_position are set. */
    do {
	if (match_length > len)
	    match_length = len;	/* match_length may be spuriously long near
				 * the end of text. */
	if (match_length <= THRESHOLD) {
	    match_length = 1;	/* Not long enough match.  Send one byte. */
	    code_buf[0] |= mask;/* 'send one byte' flag */
	    code_buf[code_buf_ptr++] = text_buf[r];	/* Send uncoded. */
	} else {
	    code_buf[code_buf_ptr++] = (unsigned char) match_position;
	    code_buf[code_buf_ptr++] = (unsigned char)
		(((match_position >> 4) & 0xf0)
		 | (match_length - (THRESHOLD + 1)));	/* Send position and
							 * length pair. Note
							 * match_length >
							 * THRESHOLD. */
	}
	if ((mask <<= 1) == 0) {/* Shift mask left one bit. */
	    memcpy(&outbuf[oindex], code_buf, code_buf_ptr);
	    oindex += code_buf_ptr;
	    codesize += code_buf_ptr;
	    code_buf[0] = 0;
	    code_buf_ptr = mask = 1;
	}
	last_match_length = match_length;
	for (i = 0; i < last_match_length && (lindex < buflen); i++) {
	    c = inbuf[lindex++];
	    CHECKSUM ^= c;
	    DeleteNode(s);	/* Delete old strings and */
	    text_buf[s] = c;	/* read new bytes */
	    if (s < F - 1)
		text_buf[s + N] = c;	/* If the position is near the end of
					 * buffer, extend the buffer to make
					 * string comparison easier. */
	    s = (s + 1) & (N - 1);
	    r = (r + 1) & (N - 1);
	    /*
	     * Since this is a ring buffer, increment the position modulo N.
	     */
	    InsertNode(r);	/* Register the string in text_buf[r..r+F-1] */
	}
	if ((textsize += i) > printcount) {
	    if (verbose) printf("%12ld\r", textsize);
	    printcount += 1024;
	    /*
	     * Reports progress each time the textsize exceeds multiples of
	     * 1024.
	     */
	}
	while (i++ < last_match_length) {	/* After the end of text, */
	    DeleteNode(s);	/* no need to read, but */
	    s = (s + 1) & (N - 1);
	    r = (r + 1) & (N - 1);
	    if (--len)
		InsertNode(r);	/* buffer may not be empty. */
	}
    } while (len > 0);		/* until length of string to be processed is
				 * zero */
    if (code_buf_ptr > 1) {	/* Send remaining code. */
	memcpy(&outbuf[oindex], code_buf, code_buf_ptr);
	oindex += code_buf_ptr;
	codesize += code_buf_ptr;
    }
    outbuf[oindex++] = EOP;
    if (verbose) {
        printf("Uncoded stream length: %ld bytes\n", textsize);	/* Encoding is done. */
        printf("Coded stream length: %ld bytes\n", codesize);
        printf("Compression Ratio: %.3f\n", (double) textsize / codesize);
    }
}


int
lzss(inbuf, outbuf, len, comp)
    char           *inbuf;
    char           *outbuf;
    int             len;
    int             comp;
{
    int             index;

    textsize = 0;		/* text size counter */
    codesize = 0;		/* code size counter */
    printcount = 0;		/* counter for reporting progress every 1K */

    if (comp) {
	index = sizeof(packet_header);
	Encode(inbuf, outbuf, len, index);
	if (PutPacketInfo(outbuf)) {
	    printf("Error:couldn't write packet header\n");
	}
    }
    return (codesize);
}

#include "ansidecl.h"

#ifdef ANSI_PROTOTYPES
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include "bfd.h"

#define min(A, B) (((A) < (B)) ? (A) : (B))
#define LOAD_ADDRESS 0


dump(section_address, buffer, count)
    int             section_address;
    unsigned char  *buffer;
    int             count;
{
    int             i;

    for (i = 0; i < count; i += 4) {
	fprintf(dumpfile, "\t.word\t0x%02x%02x%02x%02x\n",
		buffer[i], buffer[i + 1], buffer[i + 2], buffer[i + 3]);
    }
}

int
bfd_load(fname, comp)
    char           *fname;
    int             comp;
{
    int             cc, c;
    unsigned char   buf[10];
    asection       *section;
    bfd            *pbfd;
    unsigned long   entry;
    char           *sec_buf;
    char           *lzss_buf;

    pbfd = bfd_openr(fname, 0);

    if (pbfd == NULL) {
	printf("open of %s failed\n", fname);
	return (0);
    }
    if (!bfd_check_format(pbfd, bfd_object)) {
	printf("file %s  doesn't seem to be an object file\n", fname);
	return (0);
    }
    if (verbose) printf("loading %s:", fname);
    fprintf(dumpfile, "\t .text\n");
    for (section = pbfd->sections; section; section = section->next) {
	if (bfd_get_section_flags(pbfd, section) & SEC_ALLOC) {
	    bfd_vma         section_address;
	    unsigned long   section_size;
	    const char     *section_name;

	    section_name = bfd_get_section_name(pbfd, section);

	    section_address = bfd_get_section_vma(pbfd, section);
	    /*
	     * Adjust sections from a.out files, since they don't carry their
	     * addresses with.
	     */
	    if (bfd_get_flavour(pbfd) == bfd_target_aout_flavour)
		section_address += bfd_get_start_address(pbfd);
	    section_size = bfd_section_size(pbfd, section);

	    if (verbose) printf("\nsection %s at 0x%08lx (%ld bytes)",
		   section_name, section_address, section_size);

	    /* Text, data or lit */
	    if (bfd_get_section_flags(pbfd, section) & SEC_LOAD) {
		file_ptr        fptr;

		/* Register section in table */
		secarr[secnum].paddr = section_address;
		secarr[secnum].len = section_size;
		secarr[secnum].comp = comp;
		strcpy(secarr[secnum].name, section_name);

		/* Add section entry in dump file */
		fprintf(dumpfile, "\n\t.global _section%1d\n", secnum);
		fprintf(dumpfile, "_section%1d:\n", secnum);

		fptr = 0;

		/* Get buffers if compressing */
		if (comp) {
		    sec_buf = (char *) malloc(section_size);
		    lzss_buf = (char *) malloc(section_size + 200);
		}
		while (section_size > 0) {
		    char            buffer[1024];
		    int             count;

		    count = min(section_size, 1024);

		    bfd_get_section_contents(pbfd, section, buffer, fptr, count);

		    if (comp)
			memcpy(&sec_buf[fptr], buffer, count);
		    else
			dump(section_address, buffer, count);

		    section_address += count;
		    fptr += count;
		    section_size -= count;
		}
		secnum++;
		if (comp) {
		    if (verbose) printf("\n");
		    fptr = lzss(sec_buf, lzss_buf, fptr, 1);
		    dump(section_address, lzss_buf, fptr+13);
		    free(sec_buf);
		    free(lzss_buf);
		}
	    } else		/* BSS */
		if (verbose) printf("(not loaded)");
	}
    }
    if (verbose) printf("\n");

    /*
     * entry = bfd_get_start_address (pbfd);
     * 
     * printf ("[Starting %s at 0x%lx]\n", fname, entry);
     */
    return (0);
}
