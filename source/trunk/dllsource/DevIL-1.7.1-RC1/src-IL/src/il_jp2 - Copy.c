//-----------------------------------------------------------------------------
//
// ImageLib Sources
// Copyright (C) 2000-2008 by Denton Woods
// Last modified: 08/24/2008
//
// Filename: src-IL/src/il_jp2.c
//
// Description: Jpeg-2000 (.jp2) functions
//
//-----------------------------------------------------------------------------


#include "il_internal.h"
#ifndef IL_NO_JP2
#include <jasper/jasper.h>
#include "il_jp2.h"

//! Reads an icon file.
ILboolean ilLoadJp2(ILconst_string FileName)
{
	ILHANDLE		Jp2File;
	ILboolean		bJp2 = IL_FALSE;
	ILint			Test, x, y, c;
	jas_matrix_t	*origdata;

	/*Jp2File = iopenr(FileName);
	if (Jp2File == NULL) {
		ilSetError(IL_COULD_NOT_OPEN_FILE);
		return bJp2;
	}

	bJp2 = ilLoadJp2F(Jp2File);
	icloser(Jp2File);*/

	jas_stream_t	*Stream;
	jas_image_t		*Jp2Image = NULL;
	int				cmptlut[3];

	if (jas_init())
	{
		ilSetError(IL_LIB_JP2_ERROR);
		return IL_FALSE;
	}
	if (!(Stream = jas_stream_fopen(FileName, "rb")))
	{
		ilSetError(IL_COULD_NOT_OPEN_FILE);
		return IL_FALSE;
	}

	// Decode image
	if (!(Jp2Image = jas_image_decode(Stream, -1, 0)))
	{
		ilSetError(IL_ILLEGAL_FILE_VALUE);
		jas_stream_close(Stream);
		return IL_FALSE;
	}

	// Close the input stream.
	jas_stream_close(Stream);

	// We're not supporting anything other than 8 bits/component yet.
	if (jas_image_cmptprec(Jp2Image, 0) != 8)
	{
		jas_image_destroy(Jp2Image);
		ilSetError(IL_ILLEGAL_FILE_VALUE);
		return IL_FALSE;
	}

	switch (jas_image_numcmpts(Jp2Image))
	{
		case 3:
			ilTexImage(jas_image_width(Jp2Image), jas_image_height(Jp2Image), 1, 3, IL_RGB, IL_UNSIGNED_BYTE, NULL);
			break;
		case 4:
			ilTexImage(jas_image_width(Jp2Image), jas_image_height(Jp2Image), 1, 4, IL_RGBA, IL_UNSIGNED_BYTE, NULL);
			break;
		default:
			jas_image_destroy(Jp2Image);
			ilSetError(IL_ILLEGAL_FILE_VALUE);
			return IL_FALSE;
	}
	iCurImage->Origin = IL_ORIGIN_UPPER_LEFT;

	/*if (1) {
		jas_image_t *NewImage;
		jas_cmprof_t *outprof;
		// forcing conversion to sRGB
		if (!(outprof = jas_cmprof_createfromclrspc(JAS_CLRSPC_SRGB))) {
			// cannot create sRGB profile
			return IL_FALSE;
		}
		if (!(NewImage = jas_image_chclrspc(Jp2Image, outprof, JAS_CMXFORM_INTENT_PER))) {
			//jas_eprintf("cannot convert to sRGB\n");
			return IL_FALSE;
		}
		jas_image_destroy(Jp2Image);
		jas_cmprof_destroy(outprof);
		Jp2Image = NewImage;
	}

	if ((cmptlut[0] = jas_image_getcmptbytype(Jp2Image,
	  JAS_IMAGE_CT_COLOR(JAS_CLRSPC_CHANIND_RGB_R))) < 0 ||
	  (cmptlut[1] = jas_image_getcmptbytype(Jp2Image,
	  JAS_IMAGE_CT_COLOR(JAS_CLRSPC_CHANIND_RGB_G))) < 0 ||
	  (cmptlut[2] = jas_image_getcmptbytype(Jp2Image,
	  JAS_IMAGE_CT_COLOR(JAS_CLRSPC_CHANIND_RGB_B))) < 0)
		return IL_FALSE;

	for (c = 0; c < 3; c++)
	{
		for (y = 0; y < iCurImage->Height; y++)
		{
			for (x = 0; x < iCurImage->Width; x++)
			{
				iCurImage->Data[y * iCurImage->Width * 3 + x * 3 + c] = jas_image_readcmptsample(Jp2Image, cmptlut[c], x, y);
			}
		}
	}*/

	for (c = 0; c < iCurImage->Bpp; c++)
	{
		if (!(origdata = jas_matrix_create(iCurImage->Height, iCurImage->Width)))
		{
			ilSetError(IL_LIB_JP2_ERROR);
			return IL_FALSE;  // @TODO: Error
		}
		if (jas_image_readcmpt(Jp2Image, c, 0, 0, iCurImage->Width, iCurImage->Height, origdata))
		{
			return IL_FALSE;
		}

		for (y = 0; y < iCurImage->Height; y++)
		{
			for (x = 0; x < iCurImage->Width; x++)
			{
				iCurImage->Data[y * iCurImage->Width * 3 + x * 3 + c] = origdata->data_[y * origdata->numcols_ + x];
			}
		}

		jas_matrix_destroy(origdata);
	}

	jas_image_destroy(Jp2Image);

	ilFixImage();

	return IL_TRUE;

//	return bJp2;
}


//! Reads an already-opened icon file.
ILboolean ilLoadJp2F(ILHANDLE File)
{
	ILuint		FirstPos;
	ILboolean	bRet;

	iSetInputFile(File);
	FirstPos = itell();
	bRet = iLoadJp2Internal();
	iseek(FirstPos, IL_SEEK_SET);

	return bRet;
}


//! Reads from a memory "lump" that contains an icon.
ILboolean ilLoadJp2L(const ILvoid *Lump, ILuint Size)
{
	iSetInputLump(Lump, Size);
	return iLoadJp2Internal();
}


// Internal function used to load the icon.
ILboolean iLoadJp2Internal()
{
	/*jas_image_t *Jp2Image = NULL;

	if (jas_init())
	{
		ilSetError(IL_LIB_JP2_ERROR);
		return IL_FALSE;
	}


	jas_image_destroy(Jp2Image);

	ilFixImage();

	return IL_TRUE;*/

	return IL_FALSE;
}


#endif//IL_NO_JP2
