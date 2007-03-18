/*
 * VTFEdit
 * Copyright (C) 2005 Neil Jedrzejewski & Ryan Gregg
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#pragma once

#include "stdafx.h"
#include "VTFOptions.h"

using namespace System;

namespace VTFEdit
{
	__gc class CVTFFileUtility
	{
	public:
		static SVTFCreateOptions GetCreateOptions(CVTFOptions *Options)
		{
			SVTFCreateOptions VTFCreateOptions;

			vlImageCreateDefaultCreateStructure(&VTFCreateOptions);

			VTFCreateOptions.ImageFormat = Options->NormalFormat;
			VTFCreateOptions.bResize = Options->ResizeImage;
			VTFCreateOptions.ResizeMethod = Options->ResizeMethod;
			VTFCreateOptions.ResizeFilter = Options->ResizeFilter;
			VTFCreateOptions.ResizeSharpenFilter = Options->ResizeSharpenFilter;
			VTFCreateOptions.bResizeClamp = Options->ResizeClamp;
			VTFCreateOptions.uiResizeClampWidth = Options->ResizeClampWidth;
			VTFCreateOptions.uiResizeClampHeight = Options->ResizeClampHeight;
			VTFCreateOptions.bGammaCorrection = Options->CorrectGamma;
			VTFCreateOptions.sGammaCorrection = Options->GammaCorrection;
			VTFCreateOptions.bMipmaps = Options->GenerateMipmaps;
			VTFCreateOptions.MipmapFilter = Options->MipmapFilter;
			VTFCreateOptions.MipmapSharpenFilter = Options->MipmapSharpenFilter;
			VTFCreateOptions.bNormalMap = Options->ConvertToNormalMap;
			VTFCreateOptions.KernelFilter = Options->KernelFilter;
			VTFCreateOptions.HeightConversionMethod = Options->HeightConversionMethod;
			VTFCreateOptions.NormalAlphaResult = Options->AlphaResult;
			VTFCreateOptions.sNormalScale = Options->NormalScale;
			VTFCreateOptions.bNormalWrap = Options->NormalWrap;
			VTFCreateOptions.bThumbnail = Options->GenerateThumbnail;
			VTFCreateOptions.bReflectivity = Options->ComputeReflectivity;
			VTFCreateOptions.bSphereMap = Options->GenerateSphereMap;

			vlSetInteger(VTFLIB_DXT_QUALITY, Options->DXTQuality);

			vlSetFloat(VTFLIB_LUMINANCE_WEIGHT_R, Options->LuminanceWeightR);
			vlSetFloat(VTFLIB_LUMINANCE_WEIGHT_G, Options->LuminanceWeightG);
			vlSetFloat(VTFLIB_LUMINANCE_WEIGHT_B, Options->LuminanceWeightB);

			vlSetFloat(VTFLIB_UNSHARPEN_RADIUS, Options->UnsharpenMaskRadius);
			vlSetFloat(VTFLIB_UNSHARPEN_AMOUNT, Options->UnsharpenMaskAmount);
			vlSetFloat(VTFLIB_UNSHARPEN_THRESHOLD, Options->UnsharpenMaskThreshold);

			vlSetFloat(VTFLIB_XSHARPEN_STRENGTH, Options->XSharpenStrength);
			vlSetFloat(VTFLIB_XSHARPEN_THRESHOLD, Options->XSharpenThreshold);

			return VTFCreateOptions;
		}
	};
}