/*
 *  Copyright (C) 2007 ARM
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see http://www.gnu.org/licenses/.
 *
 */

/* Define the RASC_STANDALONE when building this file
   as a simple standalone disassembler */

#ifndef RASC_STANDALONE
  #include "sysdep.h"
  #define STATIC_TABLE
  #define DEFINE_TABLE  
  #include "dis-asm.h"
#endif /* RASC_STANDALONE */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "rasc-opc.h"
#include "rasc-dis.h"

#define RASC_BUF_SIZE 512

static const char*mne_strings[] =
  {
#define MNE_(X) #X
    MNE_LIST
#undef MNE_
  };

typedef unsigned long long rasc_u64; /* TODO: What's the recommended 64-bit type? */

/* Extract bit field or return default_val if mask is 0 or pos<0 */
static __inline int extract_bitfield(rasc_u64 instr, int pos, unsigned size, int default_val)
{
  if (size>0 && pos>=0)
    return (instr >> pos) & ((1<<size)-1);
  return default_val;
}

static int sign_extend(int val, int from_bit)
{
  if (val & (1<<from_bit))
    val -= 2<<from_bit;
  return val;
}

/****************************************************************************/

/* Decode an instruction. */
/* Return no. of bytes consumed (or 0 on failure) */
static int decode_instr(
  const unsigned char *ibytes, int avail_bytes,
  struct rasc_idecode *decode,
  const struct idesc_params **out_iparams)
{  
  rasc_u64 instr64= 0;
  int i;
  const struct idesc_params *iparams;
  
  /* Collect a word */
  for (i= 0; i<avail_bytes && i<(int) sizeof(instr64); i++)
    instr64 |= ((rasc_u64) ibytes[i])<<(8*i);

  iparams= isa_find_iparams((rasc_u64) instr64);

  if (iparams && iparams->ibytes <= avail_bytes)
  {
    int consumed= iparams->ibytes;
    int op= extract_bitfield(instr64, iparams->op_pos, iparams->op_size, 0);
    int rd= extract_bitfield(instr64, iparams->rd.pos, REG_BITS, -1);
    int rn= extract_bitfield(instr64, iparams->rn.pos, REG_BITS, -1);
    int rm= extract_bitfield(instr64, iparams->rm.pos, REG_BITS, -1);
    int ro= extract_bitfield(instr64, iparams->ro.pos, REG_BITS, -1);
    const struct idesc_imm_field *imm_field= get_non_pcrel_field(iparams);
    const struct idesc_imm_field *range_field= get_pcrel_field(iparams);
    int imm_val= -1;
    int range_val= -1, range_is_icnt= 0, range_is_vskip= 0;

    if (iparams->rd_carries_opcode)
      op= 2*op + (rd>rn);    
    if (iparams->rd.rfc == RFC_IMPLIED_R15) rd= 15;
    if (iparams->rn.rfc == RFC_IMPLIED_R15) rn= 15;
    if (iparams->rm.rfc == RFC_IMPLIED_R15) rm= 15;
    if (iparams->ro.rfc == RFC_IMPLIED_R15) ro= 15;
  
    if (imm_field)
    {
      int suffix_bytes;
      imm_val= extract_bitfield(instr64, imm_field->pos, imm_field->size, -1);

      /* Check if code matches a suffixed mode */
      suffix_bytes= isa_get_imm_suffix_bytes(imm_field, imm_val);

      /* Handle immediate suffix */
      if (suffix_bytes)
      {
        int i;
        if (consumed+suffix_bytes >avail_bytes)
          return 0; /* does not fit */
        
        imm_val= 0;
        for (i= 0; i<suffix_bytes; i++)
          imm_val |= ((unsigned) ibytes[consumed+i])<<(8*i);
        consumed += suffix_bytes;        
      }

      /* TODO: It would be more coding efficent if both assembler/linker and disassembler 
               would use factor and bias also for suffixes */
      if (!suffix_bytes)
      {
        imm_val *= imm_field->factor; /* normally 1 */
        imm_val += imm_field->bias;   /* normally 0 */
      }
    }
  
    if (range_field)
    {
      int suffix_bytes= 0;
      /* unsigned extraction */
      /* After factor/bias has been applied here, the dissassembler and      */
      /* simulator can simply use (iaddr + range_val) as the branch target. */      
      /* (or skip instructions if range_is_icnt is set) */
      range_val= extract_bitfield(instr64, range_field->pos, range_field->size, -1);          

      /* Check if code matches a suffixed mode */
      /* Special extension for BCC3 */
      suffix_bytes= isa_get_branch_suffix_bytes(range_field->size, range_val);
      
      if (range_field->size==3)
        range_is_icnt= (suffix_bytes==0);
      
      /* Kludge: The range_field is currently set to non-pc relative for VSKIP */ 
      range_is_vskip= !range_field->pcrel; /* detect vskip kludge */
      
      if (suffix_bytes)
      {
        int i;
        if (consumed+suffix_bytes >avail_bytes)
          return 0; /* does not fit */
                
        range_val= 0;
        for (i= 0; i<suffix_bytes; i++)
          range_val |= ((unsigned) ibytes[consumed+i])<<(8*i);        
        consumed += suffix_bytes;        
        
        /* Suffixed branches are sign extended */
        range_val= sign_extend(range_val, 8*suffix_bytes-1);
	range_val += consumed-suffix_bytes;          
      }
      else if (range_is_icnt)
      {
        range_val++;
      }
      else
      {
        range_val *= range_field->factor;
        range_val += range_field->bias;
      }
    }
    
    strcpy(decode->mne, mne_strings[iparams->mnes[op]]);
    decode->mne_idx = iparams->mnes[op];
    decode->rd= iparams->rd.rfc == RFC_VREG? -1 : rd;
    decode->rn= iparams->rn.rfc == RFC_VREG? -1 : rn;
    decode->rm= iparams->rm.rfc == RFC_VREG? -1 : rm;
    decode->ro= iparams->ro.rfc == RFC_VREG? -1 : ro;
    decode->vd= iparams->rd.rfc == RFC_VREG? rd : -1;
    decode->vn= iparams->rn.rfc == RFC_VREG? rn : -1;
    decode->vm= iparams->rm.rfc == RFC_VREG? rm : -1;
    decode->vo= iparams->ro.rfc == RFC_VREG? ro : -1;
    decode->register_list= (iparams->format == FMT_LIST);
    decode->post_increment= (iparams->format == FMT_BRACKET_PLUS_PLUS);
    decode->imm_present= (imm_field != 0);
    decode->imm_val= imm_val;
    decode->range_present= (range_field != 0);
    decode->range_val= range_val;
    decode->range_is_icnt= range_is_icnt;
    decode->range_is_vskip= range_is_vskip;

    /* vsbm kludge: */    
    /* This is done because we don't have any clean support for the split 8-bit immediate */
    /* Translate "vsbm Vd,Vn,imm0" into "vsbm imm0,imm1" */
    if (!strcmp(decode->mne,"vsbm"))
    {
      decode->range_present= 1;
      decode->range_val= decode->imm_val;
      decode->range_is_icnt= 0;
      decode->range_is_vskip= 1;
      decode->imm_val= (decode->vd<<4) + decode->vn;
      decode->vd= -1;
      decode->vn= -1;
    }    

    *out_iparams= iparams;
    return consumed;
  }

  return 0;
}

/* Instruction decode function for RASC standalone use */
/* Special interface for RASC emulator.                */
/* Return no. of bytes decoded, or 0 on failure        */
int rasc_idecode(const unsigned char *ibytes, int avail_bytes,
  struct rasc_idecode *decode)
{
  const struct idesc_params *iparams;
  return decode_instr(ibytes,avail_bytes,decode,&iparams);
}

/* Check the length of suffixes */
/* Special interface for instruction decoder FLI simulation model */
void rasc_get_length_of_suffixes(unsigned instr, int *p_imm_suffix_len,
  int *p_branch_suffix_len)
{
  const struct idesc_params *iparams;
  int imm_suffix_len= 0;
  int branch_suffix_len= 0;

  /* isa_find_iparams takes a 64-bit word now but we pass
     a 32-bit word here. TODO: Does it break anything? */
  iparams= isa_find_iparams(instr);
  if (iparams)
  {
    const struct idesc_imm_field *imm_field= get_non_pcrel_field(iparams);
    const struct idesc_imm_field *range_field= get_pcrel_field(iparams);
    if (imm_field)
    {
      int imm_val= extract_bitfield(instr, imm_field->pos, imm_field->size, -1);
  
      /* Check if code matches a suffixed mode */
      imm_suffix_len= isa_get_imm_suffix_bytes(imm_field, imm_val);    
    }
    if (range_field)
    {
      /* unsigned extraction */
      /* After factor/bias has been applied here, the dissassembler and      */
      /* simulator can simply use (iaddr + range_val) as the branch target. */      
      /* (or skip instructions if range_is_icnt is set) */
      int range_val= extract_bitfield(instr, range_field->pos, range_field->size, -1);          
  
      /* Check if code matches a suffixed mode */
      /* Special extension for BCC3 */
      branch_suffix_len= isa_get_branch_suffix_bytes(range_field->size, range_val);
    }
  }
  *p_imm_suffix_len= imm_suffix_len;
  *p_branch_suffix_len= branch_suffix_len;
}


/****************************************************************************/

/* Main disassembly function                                       */
/* Disassemble single instruction to buffer                        */
/* Return no. of bytes consumed by instruction (always > 0)        */
/* Return branch target in *opt_idecode (unless opt_idecode==NULL) */
static int rasc_disasm_internal(int addr,
  const unsigned char *ibytes, int avail_bytes,
  char buf[RASC_BUF_SIZE],  
  unsigned short imm_in,  
  int *opt_btarget,
  struct rasc_idecode *opt_idecode)
{
  int consumed;  
  struct rasc_idecode decode;
  const struct idesc_params *iparams;

  if (opt_btarget)  /* Did caller provide return variable for branch target? */
    *opt_btarget= -1;

  consumed= decode_instr(ibytes,avail_bytes,&decode,&iparams);

  if (consumed)
  {
    int need_comma= 0;
    char tmp[RASC_BUF_SIZE];
    int j;
    
    /* Merge in the prefix imm_in */
    decode.imm_val |= imm_in;

    strcpy(buf, decode.mne);
    for (j= 6-strlen(buf); j>0; j--)
      strcat(buf," ");
    strcat(buf," ");

    if ((!strcmp(decode.mne,"vfst") || !strcmp(decode.mne,"vfstb"))
	&& decode.imm_present)
      {
	int n= decode.imm_val;
        if (n >= 0 && n<10)    
          sprintf(tmp,"%d,",n);   /* short form for numbers 0-9 */
        else
          sprintf(tmp,"0x%x,",(unsigned) n); /* use hex */
	strcat(buf,tmp);
      }
      
    /* Destination reg? */
    if (decode.rd>=0 || decode.vd>=0)
    {
      if (need_comma) strcat(buf,",");
      if (decode.vd>=0) sprintf(tmp,"v%d",decode.vd);
      else if (decode.rd == 15) strcpy(tmp,"sp");           
      else if (decode.rd == 14) strcpy(tmp,"lr");
      else sprintf(tmp,"r%d",decode.rd);           
      strcat(buf,tmp);
      need_comma= 1;
      if (iparams->format == FMT_INDEX0)
	{
	  strcat(buf,"[");
          sprintf(tmp,"%d", decode.imm_val);
	  strcat(buf,tmp);
	  strcat(buf,"]");
	}
    }

    if (iparams->format == FMT_BRACKET || iparams->format == FMT_BRACKET_PLUS_PLUS)
    {
      if (need_comma) strcat(buf,",");
      strcat(buf,"[");
      need_comma= 0;
    }

    /* Source reg? */
    if (decode.rn>=0 || decode.vn>=0)
    {
      if (need_comma) strcat(buf,",");
      if (decode.vn >= 0) sprintf(tmp,"v%d",decode.vn);
      else if (decode.rn == 15) strcpy(tmp,"sp");           
      else if (decode.rn == 14) strcpy(tmp,"lr");           
      else sprintf(tmp,"r%d",decode.rn);           
      strcat(buf,tmp);
      need_comma= 1;
    }

    /* Second source reg? */
    if (decode.rm>=0 || decode.vm>=0)
    {
      if (need_comma) strcat(buf,",");
      if (decode.vm >= 0) sprintf(tmp,"v%d",decode.vm);
      else if (decode.rm == 15) strcpy(tmp,"sp");           
      else if (decode.rm == 14) strcpy(tmp,"lr");           
      else sprintf(tmp,"r%d",decode.rm);
      strcat(buf,tmp);
      need_comma= 1;
    }

    /* Third source reg? */
    if (decode.ro>=0 || decode.vo>=0)
    {
      if (need_comma) strcat(buf,",");
      if (decode.vo >= 0) sprintf(tmp,"v%d",decode.vo);
      else if (decode.ro == 15) strcpy(tmp,"sp");           
      else if (decode.ro == 14) strcpy(tmp,"lr");           
      else sprintf(tmp,"r%d",decode.ro);
      strcat(buf,tmp);
      need_comma= 1;
    }
        
    /* Immediate */        
    if (((decode.imm_present && iparams->format != FMT_INDEX0) ||
	 (decode.range_present && iparams->format == FMT_INDEX0 ))
	&& strcmp(decode.mne,"vfst") && strcmp(decode.mne,"vfstb"))
    {
      int n= decode.imm_val;
      if (iparams->format == FMT_INDEX0)
        n= decode.range_val;

      if (iparams->format == FMT_INDEX) strcat(buf,"[");
      else if (need_comma) strcat(buf,",");

      if (opt_btarget && (!strcmp(decode.mne,"jsr") || !strcmp(decode.mne,"jmp")))
        /* Return a pointer. Caller appends branch target symbol */
        *opt_btarget= n; 
      else
      {      
        if (n >= 0 && n<10)    
          sprintf(tmp,"%d",n);   /* short form for numbers 0-9 */
        else
          sprintf(tmp,"0x%x",(unsigned) n); /* use hex */
        
        strcat(buf,tmp);       
        if (iparams->format == FMT_INDEX) strcat(buf,"]");
        need_comma= 1;
      }
    }

    if (iparams->format == FMT_BRACKET)
      strcat(buf,"]");
    if (iparams->format == FMT_BRACKET_PLUS_PLUS)
      strcat(buf,"++]");

    if (decode.register_list)
      strcat(buf,"-r14"); /* all register lists end with r14 :-) */

    if (decode.range_present && iparams->format != FMT_INDEX0)
    {
      int destaddr= addr + decode.range_val;
      if (need_comma)
        strcat(buf,",");

      if (decode.range_is_icnt)
      {
	if (decode.range_val == 1)
	  sprintf(tmp, "<skip 1 instruction>");
	else
	  sprintf(tmp, "<skip %d instructions>", decode.range_val);
        strcat(buf,tmp);
      }
      else if (decode.range_is_vskip)
      {
        /* Use same syntax as immediates for now. */      
        int n= decode.range_val;
        if (n >= 0 && n<10)    
          sprintf(tmp,"%d",n);   /* short form for numbers 0-9 */
        else
          sprintf(tmp,"0x%x",(unsigned) n); /* use hex */
        strcat(buf,tmp);
      }
      else if (opt_btarget)
        /* Return a pointer. Caller appends branch target symbol */
        *opt_btarget= destaddr; 
      else
      {
        sprintf(tmp,"0x%04x",destaddr);
        strcat(buf,tmp);
      }
    }
    
    if (opt_idecode)
      *opt_idecode= decode;
    
    return consumed;
  }
  else 
  {
    int bytes;

    /* Collect a word */
    unsigned instr= 0;
    int i;
    for (i= 0; i<avail_bytes && i<4; i++)
      instr |= ((unsigned) ibytes[i])<<(8*i);

    if (avail_bytes >= 2)
    {
      bytes= 2; sprintf(buf,".short 0x%04x", instr & 0xffff); 
    }
    else
    {
      bytes= 1; sprintf(buf,".byte  0x%02x", instr & 0xff); 
    }
    
    if (opt_idecode)
    {
      int i;
      for (i= 0; buf[i]!= ' '; i++)
        opt_idecode->mne[i]= i;
      opt_idecode->mne[i]= 0;
    }
    return bytes;
  }  
}

/****************************************************************************/

/* Glue together imm prefix instruction with next instruction, where possible */
static int rasc_disasm_ex(int addr,
  const unsigned char *ibytes, int avail_bytes,
  char buf[RASC_BUF_SIZE],   
  int *opt_btarget,
  struct rasc_idecode *opt_idecode)
{
  /* Disassemble an instruction */
  struct rasc_idecode decode;
  int consumed= rasc_disasm_internal(addr, ibytes, avail_bytes, buf, 0,
    opt_btarget, &decode);
  
  if (opt_idecode)
    *opt_idecode= decode;

  /* Glue together imm prefix with the next instruction */
  if (!strcmp(decode.mne,"imm") && consumed<avail_bytes)
  {
    /* Grab the immediate to be appendended with the next instruction */ 
    unsigned short imm_in= (decode.imm_val<<4);
    char rascbuf2[RASC_BUF_SIZE] = {0};
    int target2;
    struct rasc_idecode decode2;
    int next_consumed;

    /* Disassemble the next instruction */
    next_consumed= rasc_disasm_internal(addr+consumed, ibytes+consumed, avail_bytes-consumed, 
      rascbuf2, imm_in, opt_btarget?&target2:0, &decode2);
    
    if (strcmp(decode2.mne,"imm"))
    {
      /* Next is not an immediate. Print them out together */
      consumed += next_consumed;
      strcpy(buf,rascbuf2);
      if (opt_btarget) *opt_btarget= target2;
      if (opt_idecode) *opt_idecode= decode2;
    }    
    /* else next is also an immediate: too weird, don't combine */
  }
  return consumed;
}

/* Disassemble single instruction into buffer */
/* Return the no. of bytes consumed */
/* Simplified API, used by rascdisasm and rascemul */
int rasc_disasm(int addr, const unsigned char *ibytes, int avail_bytes, char buf[RASC_BUF_SIZE])
{
  return rasc_disasm_ex(addr,ibytes,avail_bytes,buf, 0, 0);
}

/****************************************************************************/

#ifndef RASC_STANDALONE

/* Max no. of bytes in an instruction (not counting immediate prefix) */
#define MAX_BYTES (16)

void (*rasc_instruction_hook)(unsigned char *ibuf, int ibytes) = 0;

/* File entry point */
int
print_insn_rasc (memaddr, info)
     bfd_vma memaddr;
     struct disassemble_info *info;
{
  unsigned char ibuf[MAX_BYTES*MAX_BYTES] = {0};
  fprintf_ftype fprintf = info->fprintf_func;
  void *stream = info->stream;
  int status= 0, consumed, read_bytes;

  info->bytes_per_chunk= 1; /* Specify how the hex dump is grouped */
  info->bytes_per_line = 6;

  /* Read as many bytes as we can into our little buffer */
  read_bytes= MAX_BYTES;
  while (read_bytes && (status = info->read_memory_func (memaddr, ibuf, read_bytes, info)))
    read_bytes--;
  
  if (!read_bytes)
  {
    info->memory_error_func(status, memaddr, info);
    return -1;
  }
  
  char rascbuf[RASC_BUF_SIZE] = {0};  
  int btarget= -1;  
  struct rasc_idecode idecode;

  /* Disassemble the instruction */ 
  consumed= rasc_disasm_ex(memaddr, ibuf, read_bytes,
    rascbuf, info->print_address_func? &btarget:0, &idecode);
  
  /* HACK: allow for extra stuff to be done here */
  if (rasc_instruction_hook)
    rasc_instruction_hook(ibuf, read_bytes);

  fprintf (stream, "%s", rascbuf);

  if (btarget != -1)
  {
    /* Use disassembler provided function to print out the branch target symbol */
    info->print_address_func(btarget, info);
  }

  return consumed; /* No. of bytes consumed */
}

#endif /* RASC_STANDALONE */
