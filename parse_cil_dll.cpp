#include <string>
#include <stdlib.h>
#include <cstring>
#include <stdio.h>
#include <assert.h>
#include "mono_cil-coff.h"

#if _MSC_VER
#define snprintf _snprintf
#endif

struct MonoStreamHeader
{
    const char* data;
    guint32 size;
};

struct VMImage
{
    char *raw_data;
    guint32 raw_data_len;
    char *raw_metadata;

    gint16 md_version_major, md_version_minor;

    MonoStreamHeader heap_strings;
    MonoStreamHeader heap_us;
    MonoStreamHeader heap_blob;
    MonoStreamHeader heap_guid;
    MonoStreamHeader heap_tables;

    std::string version;

    bool uncompressed_metadata;
};
typedef enum {
        MONO_IMAGE_OK,
        MONO_IMAGE_ERROR_ERRNO,
        MONO_IMAGE_MISSING_ASSEMBLYREF,
        MONO_IMAGE_IMAGE_INVALID
} MonoImageOpenStatus;
struct GSList;

#define g_new0(type, num) static_cast<type*>(calloc(num, sizeof(type)));


#if NO_UNALIGNED_ACCESS

guint16 mono_read16 (const unsigned char *x);
guint32 mono_read32 (const unsigned char *x);
guint64 mono_read64 (const unsigned char *x);

#define read16(x) (mono_read16 ((const unsigned char *)(x)))
#define read32(x) (mono_read32 ((const unsigned char *)(x)))
#define read64(x) (mono_read64 ((const unsigned char *)(x)))

#else

#define read16(x) GUINT16_FROM_LE (*((const guint16 *) (x)))
#define read32(x) GUINT32_FROM_LE (*((const guint32 *) (x)))
#define read64(x) GUINT64_FROM_LE (*((const guint64 *) (x)))

#endif

typedef union {
char c [2];
guint16 i;
} mono_rint16;

typedef union {
char c [4];
guint32 i;
} mono_rint32;

typedef union {
char c [8];
guint64 i;
} mono_rint64;

guint16
mono_read16 (const unsigned char *x)
{
mono_rint16 r;
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
r.c [0] = x [0];
r.c [1] = x [1];
#else
r.c [1] = x [0];
r.c [0] = x [1];
#endif
return r.i;
}

guint32
mono_read32 (const unsigned char *x)
{
mono_rint32 r;
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
r.c [0] = x [0];
r.c [1] = x [1];
r.c [2] = x [2];
r.c [3] = x [3];
#else
r.c [3] = x [0];
r.c [2] = x [1];
r.c [1] = x [2];
r.c [0] = x [3];
#endif
return r.i;
}

guint64
mono_read64 (const unsigned char *x)
{
mono_rint64 r;
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
r.c [0] = x [0];
r.c [1] = x [1];
r.c [2] = x [2];
r.c [3] = x [3];
r.c [4] = x [4];
r.c [5] = x [5];
r.c [6] = x [6];
r.c [7] = x [7];
#else
r.c [7] = x [0];
r.c [6] = x [1];
r.c [5] = x [2];
r.c [4] = x [3];
r.c [3] = x [4];
r.c [2] = x [5];
r.c [1] = x [6];
r.c [0] = x [7];
#endif
return r.i;
}

#define TRUE true
#define FALSE false

#define G_BYTE_ORDER 0
#define G_LITTLE_ENDIAN 0

#define GUINT16_SWAP_LE_BE_CONSTANT(x) ((((guint16) x) >> 8) | ((((guint16) x) << 8)))

#define GUINT16_SWAP_LE_BE(x) ((guint16) (((guint16) x) >> 8) | ((((guint16)(x)) & 0xff) << 8))
#define GUINT32_SWAP_LE_BE(x) ((guint32) \
( (((guint32) (x)) << 24)| \
((((guint32) (x)) & 0xff0000) >> 8) | \
((((guint32) (x)) & 0xff00) << 8) | \
(((guint32) (x)) >> 24)) )
 
#define GUINT64_SWAP_LE_BE(x) ((guint64) (((guint64)(GUINT32_SWAP_LE_BE(((guint64)x) & 0xffffffff))) << 32) | \
GUINT32_SWAP_LE_BE(((guint64)x) >> 32))



 
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
# define GUINT64_FROM_BE(x) GUINT64_SWAP_LE_BE(x)
# define GUINT32_FROM_BE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT16_FROM_BE(x) GUINT16_SWAP_LE_BE(x)
# define GUINT_FROM_BE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT64_FROM_LE(x) (x)
# define GUINT32_FROM_LE(x) (x)
# define GUINT16_FROM_LE(x) (x)
# define GUINT_FROM_LE(x) (x)
# define GUINT64_TO_BE(x) GUINT64_SWAP_LE_BE(x)
# define GUINT32_TO_BE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT16_TO_BE(x) GUINT16_SWAP_LE_BE(x)
# define GUINT_TO_BE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT64_TO_LE(x) (x)
# define GUINT32_TO_LE(x) (x)
# define GUINT16_TO_LE(x) (x)
# define GUINT_TO_LE(x) (x)
#else
# define GUINT64_FROM_BE(x) (x)
# define GUINT32_FROM_BE(x) (x)
# define GUINT16_FROM_BE(x) (x)
# define GUINT_FROM_BE(x) (x)
# define GUINT64_FROM_LE(x) GUINT64_SWAP_LE_BE(x)
# define GUINT32_FROM_LE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT16_FROM_LE(x) GUINT16_SWAP_LE_BE(x)
# define GUINT_FROM_LE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT64_TO_BE(x) (x)
# define GUINT32_TO_BE(x) (x)
# define GUINT16_TO_BE(x) (x)
# define GUINT_TO_BE(x) (x)
# define GUINT64_TO_LE(x) GUINT64_SWAP_LE_BE(x)
# define GUINT32_TO_LE(x) GUINT32_SWAP_LE_BE(x)
# define GUINT16_TO_LE(x) GUINT16_SWAP_LE_BE(x)
# define GUINT_TO_LE(x) GUINT32_SWAP_LE_BE(x)
#endif

#define INVALID_ADDRESS 0xffffffff

guint32
mono_cli_rva_image_map (MonoCLIImageInfo *iinfo, guint32 addr)
{
	const int top = iinfo->cli_section_count;
	MonoSectionTable *tables = iinfo->cli_section_tables;
	int i;
	
	for (i = 0; i < top; i++){
		if ((addr >= tables->st_virtual_address) &&
		    (addr < tables->st_virtual_address + tables->st_raw_data_size)){
#ifdef HOST_WIN32
			if (image->is_module_handle)
				return addr;
#endif
			return addr - tables->st_virtual_address + tables->st_raw_data_ptr;
		}
		tables++;
	}
	return INVALID_ADDRESS;
}

static gboolean
load_cli_header (MonoCLIImageInfo *iinfo, guchar* raw_data, size_t raw_data_length)
{
	guint32 offset;
	
	offset = mono_cli_rva_image_map (iinfo, iinfo->cli_header.datadir.pe_cli_header.rva);
	if (offset == INVALID_ADDRESS)
		return FALSE;

	if (offset + sizeof (MonoCLIHeader) > raw_data_length)
		return FALSE;
	memcpy (&iinfo->cli_cli_header, raw_data + offset, sizeof (MonoCLIHeader));

#if G_BYTE_ORDER != G_LITTLE_ENDIAN
#define SWAP32(x) (x) = GUINT32_FROM_LE ((x))
#define SWAP16(x) (x) = GUINT16_FROM_LE ((x))
#define SWAPPDE(x) do { (x).rva = GUINT32_FROM_LE ((x).rva); (x).size = GUINT32_FROM_LE ((x).size);} while (0)
	SWAP32 (iinfo->cli_cli_header.ch_size);
	SWAP32 (iinfo->cli_cli_header.ch_flags);
	SWAP32 (iinfo->cli_cli_header.ch_entry_point);
	SWAP16 (iinfo->cli_cli_header.ch_runtime_major);
	SWAP16 (iinfo->cli_cli_header.ch_runtime_minor);
	SWAPPDE (iinfo->cli_cli_header.ch_metadata);
	SWAPPDE (iinfo->cli_cli_header.ch_resources);
	SWAPPDE (iinfo->cli_cli_header.ch_strong_name);
	SWAPPDE (iinfo->cli_cli_header.ch_code_manager_table);
	SWAPPDE (iinfo->cli_cli_header.ch_vtable_fixups);
	SWAPPDE (iinfo->cli_cli_header.ch_export_address_table_jumps);
	SWAPPDE (iinfo->cli_cli_header.ch_eeinfo_table);
	SWAPPDE (iinfo->cli_cli_header.ch_helper_table);
	SWAPPDE (iinfo->cli_cli_header.ch_dynamic_info);
	SWAPPDE (iinfo->cli_cli_header.ch_delay_load_info);
	SWAPPDE (iinfo->cli_cli_header.ch_module_image);
	SWAPPDE (iinfo->cli_cli_header.ch_external_fixups);
	SWAPPDE (iinfo->cli_cli_header.ch_ridmap);
	SWAPPDE (iinfo->cli_cli_header.ch_debug_map);
	SWAPPDE (iinfo->cli_cli_header.ch_ip_map);
#undef SWAP32
#undef SWAP16
#undef SWAPPDE
#endif
	/* Catch new uses of the fields that are supposed to be zero */

	if ((iinfo->cli_cli_header.ch_eeinfo_table.rva != 0) ||
	    (iinfo->cli_cli_header.ch_helper_table.rva != 0) ||
	    (iinfo->cli_cli_header.ch_dynamic_info.rva != 0) ||
	    (iinfo->cli_cli_header.ch_delay_load_info.rva != 0) ||
	    (iinfo->cli_cli_header.ch_module_image.rva != 0) ||
	    (iinfo->cli_cli_header.ch_external_fixups.rva != 0) ||
	    (iinfo->cli_cli_header.ch_ridmap.rva != 0) ||
	    (iinfo->cli_cli_header.ch_debug_map.rva != 0) ||
	    (iinfo->cli_cli_header.ch_ip_map.rva != 0)){

		/*
		 * No need to scare people who are testing this, I am just
		 * labelling this as a LAMESPEC
		 */
		/* g_warning ("Some fields in the CLI header which should have been zero are not zero"); */

	}
	    
	return TRUE;
}


#if G_BYTE_ORDER != G_LITTLE_ENDIAN
#define SWAP64(x) (x) = GUINT64_FROM_LE ((x))
#define SWAP32(x) (x) = GUINT32_FROM_LE ((x))
#define SWAP16(x) (x) = GUINT16_FROM_LE ((x))
#define SWAPPDE(x) do { (x).rva = GUINT32_FROM_LE ((x).rva); (x).size = GUINT32_FROM_LE ((x).size);} while (0)
#else
#define SWAP64(x)
#define SWAP32(x)
#define SWAP16(x)
#define SWAPPDE(x)
#endif

/*
 * Returns < 0 to indicate an error.
 */
static int
do_load_header (MonoDotNetHeader *header, guchar* raw_data, size_t raw_data_length, int offset)
{
	MonoDotNetHeader64 header64;

#ifdef HOST_WIN32
	if (!image->is_module_handle)
#endif
	if (offset + sizeof (MonoDotNetHeader32) > raw_data_length)
		return -1;

	memcpy (header, raw_data + offset, sizeof (MonoDotNetHeader));

	if (header->pesig [0] != 'P' || header->pesig [1] != 'E')
		return -1;

	/* endian swap the fields common between PE and PE+ */
	SWAP32 (header->coff.coff_time);
	SWAP32 (header->coff.coff_symptr);
	SWAP32 (header->coff.coff_symcount);
	SWAP16 (header->coff.coff_machine);
	SWAP16 (header->coff.coff_sections);
	SWAP16 (header->coff.coff_opt_header_size);
	SWAP16 (header->coff.coff_attributes);
	/* MonoPEHeader */
	SWAP32 (header->pe.pe_code_size);
	SWAP32 (header->pe.pe_uninit_data_size);
	SWAP32 (header->pe.pe_rva_entry_point);
	SWAP32 (header->pe.pe_rva_code_base);
	SWAP32 (header->pe.pe_rva_data_base);
	SWAP16 (header->pe.pe_magic);

	/* now we are ready for the basic tests */

	if (header->pe.pe_magic == 0x10B) {
		offset += sizeof (MonoDotNetHeader);
		SWAP32 (header->pe.pe_data_size);
		if (header->coff.coff_opt_header_size != (sizeof (MonoDotNetHeader) - sizeof (MonoCOFFHeader) - 4))
			return -1;

		SWAP32	(header->nt.pe_image_base); 	/* must be 0x400000 */
		SWAP32	(header->nt.pe_stack_reserve);
		SWAP32	(header->nt.pe_stack_commit);
		SWAP32	(header->nt.pe_heap_reserve);
		SWAP32	(header->nt.pe_heap_commit);
	} else if (header->pe.pe_magic == 0x20B) {
		/* PE32+ file format */
		if (header->coff.coff_opt_header_size != (sizeof (MonoDotNetHeader64) - sizeof (MonoCOFFHeader) - 4))
			return -1;
		memcpy (&header64, raw_data + offset, sizeof (MonoDotNetHeader64));
		offset += sizeof (MonoDotNetHeader64);
		/* copy the fields already swapped. the last field, pe_data_size, is missing */
		memcpy (&header64, header, sizeof (MonoDotNetHeader) - 4);
		/* FIXME: we lose bits here, but we don't use this stuff internally, so we don't care much.
		 * will be fixed when we change MonoDotNetHeader to not match the 32 bit variant
		 */
		SWAP64	(header64.nt.pe_image_base);
		header->nt.pe_image_base = header64.nt.pe_image_base;
		SWAP64	(header64.nt.pe_stack_reserve);
		header->nt.pe_stack_reserve = header64.nt.pe_stack_reserve;
		SWAP64	(header64.nt.pe_stack_commit);
		header->nt.pe_stack_commit = header64.nt.pe_stack_commit;
		SWAP64	(header64.nt.pe_heap_reserve);
		header->nt.pe_heap_reserve = header64.nt.pe_heap_reserve;
		SWAP64	(header64.nt.pe_heap_commit);
		header->nt.pe_heap_commit = header64.nt.pe_heap_commit;

		header->nt.pe_section_align = header64.nt.pe_section_align;
		header->nt.pe_file_alignment = header64.nt.pe_file_alignment;
		header->nt.pe_os_major = header64.nt.pe_os_major;
		header->nt.pe_os_minor = header64.nt.pe_os_minor;
		header->nt.pe_user_major = header64.nt.pe_user_major;
		header->nt.pe_user_minor = header64.nt.pe_user_minor;
		header->nt.pe_subsys_major = header64.nt.pe_subsys_major;
		header->nt.pe_subsys_minor = header64.nt.pe_subsys_minor;
		header->nt.pe_reserved_1 = header64.nt.pe_reserved_1;
		header->nt.pe_image_size = header64.nt.pe_image_size;
		header->nt.pe_header_size = header64.nt.pe_header_size;
		header->nt.pe_checksum = header64.nt.pe_checksum;
		header->nt.pe_subsys_required = header64.nt.pe_subsys_required;
		header->nt.pe_dll_flags = header64.nt.pe_dll_flags;
		header->nt.pe_loader_flags = header64.nt.pe_loader_flags;
		header->nt.pe_data_dir_count = header64.nt.pe_data_dir_count;

		/* copy the datadir */
		memcpy (&header->datadir, &header64.datadir, sizeof (MonoPEDatadir));
	} else {
		return -1;
	}

	/* MonoPEHeaderNT: not used yet */
	SWAP32	(header->nt.pe_section_align);       /* must be 8192 */
	SWAP32	(header->nt.pe_file_alignment);      /* must be 512 or 4096 */
	SWAP16	(header->nt.pe_os_major);            /* must be 4 */
	SWAP16	(header->nt.pe_os_minor);            /* must be 0 */
	SWAP16	(header->nt.pe_user_major);
	SWAP16	(header->nt.pe_user_minor);
	SWAP16	(header->nt.pe_subsys_major);
	SWAP16	(header->nt.pe_subsys_minor);
	SWAP32	(header->nt.pe_reserved_1);
	SWAP32	(header->nt.pe_image_size);
	SWAP32	(header->nt.pe_header_size);
	SWAP32	(header->nt.pe_checksum);
	SWAP16	(header->nt.pe_subsys_required);
	SWAP16	(header->nt.pe_dll_flags);
	SWAP32	(header->nt.pe_loader_flags);
	SWAP32	(header->nt.pe_data_dir_count);

	/* MonoDotNetHeader: mostly unused */
	SWAPPDE (header->datadir.pe_export_table);
	SWAPPDE (header->datadir.pe_import_table);
	SWAPPDE (header->datadir.pe_resource_table);
	SWAPPDE (header->datadir.pe_exception_table);
	SWAPPDE (header->datadir.pe_certificate_table);
	SWAPPDE (header->datadir.pe_reloc_table);
	SWAPPDE (header->datadir.pe_debug);
	SWAPPDE (header->datadir.pe_copyright);
	SWAPPDE (header->datadir.pe_global_ptr);
	SWAPPDE (header->datadir.pe_tls_table);
	SWAPPDE (header->datadir.pe_load_config_table);
	SWAPPDE (header->datadir.pe_bound_import);
	SWAPPDE (header->datadir.pe_iat);
	SWAPPDE (header->datadir.pe_delay_import_desc);
 	SWAPPDE (header->datadir.pe_cli_header);
	SWAPPDE (header->datadir.pe_reserved);

#ifdef HOST_WIN32
	if (image->is_module_handle)
		image->raw_data_len = header->nt.pe_image_size;
#endif

	return offset;
}


static int
load_section_tables (MonoCLIImageInfo *iinfo, guchar* raw_data, size_t raw_data_length, guint32 offset)
{
	const int top = iinfo->cli_header.coff.coff_sections;
	int i;

	iinfo->cli_section_count = top;
	iinfo->cli_section_tables = g_new0 (MonoSectionTable, top);
	iinfo->cli_sections = g_new0 (void *, top);
	
	for (i = 0; i < top; i++){
		MonoSectionTable *t = &iinfo->cli_section_tables [i];

		if (offset + sizeof (MonoSectionTable) > raw_data_length)
			return FALSE;
		memcpy (t, raw_data + offset, sizeof (MonoSectionTable));
		offset += sizeof (MonoSectionTable);

#if G_BYTE_ORDER != G_LITTLE_ENDIAN
		t->st_virtual_size = GUINT32_FROM_LE (t->st_virtual_size);
		t->st_virtual_address = GUINT32_FROM_LE (t->st_virtual_address);
		t->st_raw_data_size = GUINT32_FROM_LE (t->st_raw_data_size);
		t->st_raw_data_ptr = GUINT32_FROM_LE (t->st_raw_data_ptr);
		t->st_reloc_ptr = GUINT32_FROM_LE (t->st_reloc_ptr);
		t->st_lineno_ptr = GUINT32_FROM_LE (t->st_lineno_ptr);
		t->st_reloc_count = GUINT16_FROM_LE (t->st_reloc_count);
		t->st_line_count = GUINT16_FROM_LE (t->st_line_count);
		t->st_flags = GUINT32_FROM_LE (t->st_flags);
#endif
		/* consistency checks here */
	}

	return TRUE;
}

gboolean
mono_image_load_pe_data (MonoCLIImageInfo *iinfo, MonoDotNetHeader *header, guchar* raw_data, size_t raw_data_length)
{
	MonoMSDOSHeader msdos;
	gint32 offset = 0;


#ifdef HOST_WIN32
	if (!image->is_module_handle)
#endif
	if (offset + sizeof (msdos) > raw_data_length)
		goto invalid_image;
	memcpy (&msdos, raw_data + offset, sizeof (msdos));
	
	if (!(msdos.msdos_sig [0] == 'M' && msdos.msdos_sig [1] == 'Z'))
		goto invalid_image;
	
	msdos.pe_offset = GUINT32_FROM_LE (msdos.pe_offset);

	offset = msdos.pe_offset;

	offset = do_load_header (header, raw_data, raw_data_length, offset);
	if (offset < 0)
		goto invalid_image;

	/*
	 * this tests for a x86 machine type, but itanium, amd64 and others could be used, too.
	 * we skip this test.
	if (header->coff.coff_machine != 0x14c)
		goto invalid_image;
	*/

#if 0
	/*
	 * The spec says that this field should contain 6.0, but Visual Studio includes a new compiler,
	 * which produces binaries with 7.0.  From Sergey:
	 *
	 * The reason is that MSVC7 uses traditional compile/link
	 * sequence for CIL executables, and VS.NET (and Framework
	 * SDK) includes linker version 7, that puts 7.0 in this
	 * field.  That's why it's currently not possible to load VC
	 * binaries with Mono.  This field is pretty much meaningless
	 * anyway (what linker?).
	 */
	if (header->pe.pe_major != 6 || header->pe.pe_minor != 0)
		goto invalid_image;
#endif

	/*
	 * FIXME: byte swap all addresses here for header.
	 */
	
	if (!load_section_tables (iinfo, raw_data, raw_data_length, offset))
		goto invalid_image;

	return TRUE;

invalid_image:
	return FALSE;
}


std::string mono_guid_to_string (const guint8 *guid)
{
    char buffer[1024];
    int result =  snprintf(buffer, sizeof(buffer), "%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X",
    guid[3], guid[2], guid[1], guid[0],
    guid[5], guid[4],
    guid[7], guid[6],
    guid[8], guid[9],
    guid[10], guid[11], guid[12], guid[13], guid[14], guid[15]);

    return std::string(buffer);
}



static gboolean
load_metadata_ptrs (VMImage *image, MonoCLIImageInfo *iinfo)
{
	guint32 offset, size;
	guint16 streams;
	int i;
	guint32 pad;
	char *ptr;
	
	offset = mono_cli_rva_image_map (iinfo, iinfo->cli_cli_header.ch_metadata.rva);
	if (offset == INVALID_ADDRESS)
		return FALSE;

	size = iinfo->cli_cli_header.ch_metadata.size;

	if (offset + size > image->raw_data_len)
		return FALSE;
	image->raw_metadata = image->raw_data + offset;

	/* 24.2.1: Metadata root starts here */
	ptr = image->raw_metadata;

	if (strncmp (ptr, "BSJB", 4) == 0){
		guint32 version_string_len;

		ptr += 4;
		image->md_version_major = read16 (ptr);
		ptr += 2;
		image->md_version_minor = read16 (ptr);
		ptr += 6;

		version_string_len = read32 (ptr);
		ptr += 4;
		image->version = ptr;
		ptr += version_string_len;
		pad = ptr - image->raw_metadata;
		if (pad % 4)
			ptr += 4 - (pad % 4);
	} else
		return FALSE;

	/* skip over flags */
	ptr += 2;
	
	streams = read16 (ptr);
	ptr += 2;

	for (i = 0; i < streams; i++){
		if (strncmp (ptr + 8, "#~", 3) == 0){
			image->heap_tables.data = image->raw_metadata + read32 (ptr);
			image->heap_tables.size = read32 (ptr + 4);
			ptr += 8 + 3;
		} else if (strncmp (ptr + 8, "#Strings", 9) == 0){
			image->heap_strings.data = image->raw_metadata + read32 (ptr);
			image->heap_strings.size = read32 (ptr + 4);
			ptr += 8 + 9;
		} else if (strncmp (ptr + 8, "#US", 4) == 0){
			image->heap_us.data = image->raw_metadata + read32 (ptr);
			image->heap_us.size = read32 (ptr + 4);
			ptr += 8 + 4;
		} else if (strncmp (ptr + 8, "#Blob", 6) == 0){
			image->heap_blob.data = image->raw_metadata + read32 (ptr);
			image->heap_blob.size = read32 (ptr + 4);
			ptr += 8 + 6;
		} else if (strncmp (ptr + 8, "#GUID", 6) == 0){
			image->heap_guid.data = image->raw_metadata + read32 (ptr);
			image->heap_guid.size = read32 (ptr + 4);
			ptr += 8 + 6;
		} else if (strncmp (ptr + 8, "#-", 3) == 0) {
			image->heap_tables.data = image->raw_metadata + read32 (ptr);
			image->heap_tables.size = read32 (ptr + 4);
			ptr += 8 + 3;
			image->uncompressed_metadata = TRUE;
			//mono_trace (G_LOG_LEVEL_INFO, MONO_TRACE_ASSEMBLY, "Assembly '%s' has the non-standard metadata heap #-.\nRecompile it correctly (without the /incremental switch or in Release mode).\n", image->name);
		} else {
			printf ("Unknown heap type: %s\n", ptr + 8);
			ptr += 8 + strlen (ptr + 8) + 1;
		}
		pad = ptr - image->raw_metadata;
		if (pad % 4)
			ptr += 4 - (pad % 4);
	}

	assert (image->heap_guid.data);
	assert (image->heap_guid.size >= 16);

	image->guid = mono_guid_to_string ((guint8*)image->heap_guid.data);

	return TRUE;
}

/**
* mono_metadata_compute_table_bases:
* @meta: metadata context to compute table values
*
* Computes the table bases for the metadata structure.
* This is an internal function used by the image loader code.
*/
void
mono_metadata_compute_table_bases (VMImage *meta)
{
    int i;
    const char *base = meta->tables_base;

    for (i = 0; i < MONO_TABLE_NUM; i++) {
        MonoTableInfo *table = &meta->tables [i];
        if (table->rows == 0)
            continue;

        table->row_size = mono_metadata_compute_size (meta, i, &table->size_bitfield);
        table->base = base;
        base += table->rows * table->row_size;
    }
}

/*
 * Load representation of logical metadata tables, from the "#~" stream
 */
static gboolean
load_tables (VMImage *image)
{
	const char *heap_tables = image->heap_tables.data;
	const guint32 *rows;
	guint64 valid_mask, sorted_mask;
	int valid = 0, table;
	int heap_sizes;
	
	heap_sizes = heap_tables [6];
	image->idx_string_wide = ((heap_sizes & 0x01) == 1);
	image->idx_guid_wide   = ((heap_sizes & 0x02) == 2);
	image->idx_blob_wide   = ((heap_sizes & 0x04) == 4);
	
	valid_mask = read64 (heap_tables + 8);
	sorted_mask = read64 (heap_tables + 16);
	rows = (const guint32 *) (heap_tables + 24);
	
	for (table = 0; table < 64; table++){
		if ((valid_mask & ((guint64) 1 << table)) == 0){
			if (table > MONO_TABLE_LAST)
				continue;
			image->tables [table].rows = 0;
			continue;
		}
		if (table > MONO_TABLE_LAST) {
			g_warning("bits in valid must be zero above 0x2d (II - 23.1.6)");
		} else {
			image->tables [table].rows = read32 (rows);
		}
		/*if ((sorted_mask & ((guint64) 1 << table)) == 0){
			g_print ("table %s (0x%02x) is sorted\n", mono_meta_table_name (table), table);
		}*/
		rows++;
		valid++;
	}

	image->tables_base = (heap_tables + 24) + (4 * valid);

	/* They must be the same */
	g_assert ((const void *) image->tables_base == (const void *) rows);

	mono_metadata_compute_table_bases (image);
	return TRUE;
}

static gboolean
load_metadata (VMImage *image, MonoCLIImageInfo *iinfo)
{
	if (!load_metadata_ptrs (image, iinfo))
		return FALSE;

	return load_tables (image);
}

gboolean
mono_image_load_cli_data (MonoCLIImageInfo *iinfo, MonoDotNetHeader *header, guchar* raw_data, size_t raw_data_length)
{
	/* Load the CLI header */
	if (!load_cli_header (iinfo, raw_data, raw_data_length))
		return FALSE;

	if (!load_metadata (image, iinfo))
		return FALSE;

	return TRUE;
}


void
mono_image_load_names (VMImage *image)
{
#if 0
	/* modules don't have an assembly table row */
	if (image->tables [MONO_TABLE_ASSEMBLY].rows) {
		image->assembly_name = mono_metadata_string_heap (image, 
			mono_metadata_decode_row_col (&image->tables [MONO_TABLE_ASSEMBLY],
					0, MONO_ASSEMBLY_NAME));
	}

	image->module_name = mono_metadata_string_heap (image, 
			mono_metadata_decode_row_col (&image->tables [MONO_TABLE_MODULE],
					0, MONO_MODULE_NAME));
#endif
}


static void
load_modules (VMImage *image)
{
#if 0
	MonoTableInfo *t;

	if (image->modules)
		return;

	t = &image->tables [MONO_TABLE_MODULEREF];
	image->modules = g_new0 (MonoImage *, t->rows);
	image->modules_loaded = g_new0 (gboolean, t->rows);
	image->module_count = t->rows;
#endif
}

static bool
do_mono_image_load (MonoDotNetHeader *header, MonoCLIImageInfo *iinfo, MonoImageOpenStatus *status, guchar* raw_data, size_t raw_data_length,
		    gboolean care_about_cli, gboolean care_about_pecoff)
{
	;
	GSList *errors = 0;


		
	if (status)
		*status = MONO_IMAGE_IMAGE_INVALID;

	if (care_about_pecoff == false)
		goto done;

	//if (!mono_verifier_verify_pe_data (image, &errors))
	//	goto invalid_image;

	if (!mono_image_load_pe_data (iinfo, header, raw_data, raw_data_length))
		goto invalid_image;
	
	if (care_about_cli == FALSE) {
		goto done;
	}

	//if (!mono_verifier_verify_cli_data (image, &errors))
	//	goto invalid_image;

	if (!mono_image_load_cli_data (iinfo, header))
    {
		goto invalid_image;
    }

	//if (!mono_verifier_verify_table_data (image, &errors))
	//	goto invalid_image;

	//mono_image_load_names (image);

	//load_modules (image);

done:
	if (status)
		*status = MONO_IMAGE_OK;

	return true;

invalid_image:
	if (errors) {
		//MonoVerifyInfo *info = errors->data;
		//g_warning ("Could not load image %s due to %s", image->name, info->message);
		//mono_free_verify_list (errors);
	}
	return false;
}