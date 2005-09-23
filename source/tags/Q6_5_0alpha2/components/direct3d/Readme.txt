
Arne Schäpers <a.schaepers@digitalpublishing.de> has converted D3DX.
You have to use the DLLs d3dxas.dll for retail or d3dxasd.dll for
debug purposes.

File-Structure:
~~~~~~~~~~~~~~~
DirectDraw.pas  = DDraw.h + DVP.h (+ MultiMon.h)
Direct3D.pas    = D3D.h + D3DTypes.h + D3DCaps.h + D3DVec.inl + DXFile.h
Direct3DRM.pas  = D3DRM.h + D3DRMDef.h + D3DRMObj.h + D3DRMWin.h + RMXFGUID.h + RMXFTmpl.h
DirectInput.pas = DInput.h (+ DinputD.h)
DirectPlay.pas  = DPlay.h + DPLobby.h
DirectSound.pas = DSound.h
DirectMusic.pas = DLS1.h + DLS2.h + DMDLS.h + DMError.h + DMKSCtrl.h + DMusicC.h + DMusicF.h + DMusicI.h + DMusBuff.h
DirectSetup.pas = DSetup.h
D3DX.pas        = d3dx.h + d3dxcore.h + d3dxmath.h + d3dxerr.h + d3dxshapes.h + d3dxsprite.h

No Unit = dxsdk.inc, d3dxmath.inl
