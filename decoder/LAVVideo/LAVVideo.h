/*
 *      Copyright (C) 2010-2019 Hendrik Leppkes
 *      http://www.1f0.de
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#pragma once

#include <condition_variable>
#include <mutex>
#include <queue>
#include <thread>

#include "BaseTrayIcon.h"
#include "CCOutputPin.h"
#include "DecodeManager.h"
#include "FloatingAverage.h"
#include "ILAVPinInfo.h"
#include "IMediaSideData.h"
#include "ISpecifyPropertyPages2.h"
#include "LAVPixFmtConverter.h"
#include "LAVVideoSettings.h"
#include "SynchronizedQueue.h"
#include "decoders/ILAVDecoder.h"
#include "subtitles/LAVSubtitleConsumer.h"
#include "subtitles/LAVVideoSubtitleInputPin.h"

extern "C" {
#include "libavutil/mastering_display_metadata.h"
};

#define LAVC_VIDEO_REGISTRY_KEY L"Software\\LAV\\Video"
#define LAVC_VIDEO_REGISTRY_KEY_FORMATS L"Software\\LAV\\Video\\Formats"
#define LAVC_VIDEO_REGISTRY_KEY_OUTPUT L"Software\\LAV\\Video\\Output"
#define LAVC_VIDEO_REGISTRY_KEY_HWACCEL L"Software\\LAV\\Video\\HWAccel"

#define LAVC_VIDEO_LOG_FILE     L"LAVVideo.txt"

#define DEBUG_FRAME_TIMINGS 0
#define DEBUG_PIXELCONV_TIMINGS 0

#define FILTER_PROC_MULTI_THREAD
#ifdef FILTER_PROC_MULTI_THREAD
#define FILTER_PROC_THREADS 1
#else
#define FILTER_PROC_THREADS 1
#endif
#define FRAMES_IN_THREAD 16

typedef struct {
  REFERENCE_TIME rtStart;
  REFERENCE_TIME rtStop;
} TimingCache;

typedef struct FrameInfo {
  AVFrame *in_frame;
  AVFrame *out_frames[FRAMES_IN_THREAD];
  BOOL bFramePerField;
  REFERENCE_TIME avgFrameDuration;
  REFERENCE_TIME rtDuration;
  int time_num;
  int time_den;
  int flags;
  int idx;
} FrameInfo;

class ProcThread {
 public:
  ~ProcThread() {
    if (m_pFilterGraph)
      avfilter_graph_free(&m_pFilterGraph);
    m_pFilterBufferSrc = nullptr;
    m_pFilterBufferSink = nullptr;
  }

  int Init(int i, CLAVVideo* pClav, AVPixelFormat* pix_fmt, char* strFilterInit, char* strFilterConfig, BOOL bIsHardware);
  bool Get() {
    if(!Inited || !m_bRun)
      return FALSE;
    if(!running) {
      DbgLog((LOG_TRACE, 10, L"[%d]Set thread busy", m_iIdx));
      running = TRUE;
      return TRUE;
    } else {
      return FALSE;
    }
  }
  int DeInit() {
      return 0;
  }
  int WaitDeInitDone() {
      running = TRUE;

      Inited = FALSE;
      m_bRun = FALSE;
      {
        std::unique_lock<std::mutex> lck(kickoff);
        DbgLog((LOG_TRACE, 10, L"[%d]Kickoff notify in deinit", m_iIdx));
        kickoff_cv.notify_all();
      }
      running = FALSE;
      DbgLog((LOG_TRACE, 10, L"[%d]Start join", m_iIdx));
      _thread.join();
      DbgLog((LOG_TRACE, 10, L"[%d]Join done", m_iIdx));
      if (m_pFilterGraph) {
          avfilter_graph_free(&m_pFilterGraph);
          m_pFilterGraph = NULL;
      }
      return 0;
  }

  int SetFrame(FrameInfo* info) {
    std::unique_lock<std::mutex> lck(kickoff);
    m_pFrameInfo = info;
    DbgLog((LOG_TRACE, 10, L"[%d]Kickoff notify in SetFrame", m_iIdx));
    kickoff_cv.notify_all();
    return 0;
  }

  int Start() {
    //DbgLog((LOG_TRACE, 10, L"[%d]Kickoff lock in Start", m_iIdx));
    //kickoff.lock();
    _thread = std::thread([this] { Run(); });
    return 0;
  }

  BOOL HasResult() {
    return (m_pFrameInfo && m_pFrameInfo->out_frames[0]);
  }

  FrameInfo *GetResult() {
    return m_pFrameInfo;
  }

  //std::mutex running;
  BOOL running = FALSE;
  std::mutex kickoff;
  std::condition_variable kickoff_cv;

  BOOL Inited = FALSE;

 private:
  int m_iIdx;
  CLAVVideo *m_pClav;
  FrameInfo *m_pFrameInfo = NULL;
  AVFilterGraph *m_pFilterGraph = nullptr;
  AVFilterContext *m_pFilterBufferSrc = nullptr;
  AVFilterContext *m_pFilterBufferSink = nullptr;
  BOOL m_bRun = TRUE;

  std::thread _thread;
  void Run();
};

class __declspec(uuid("EE30215D-164F-4A92-A4EB-9D4C13390F9F")) CLAVVideo : public CTransformFilter, public ISpecifyPropertyPages2,
                                                                           public ILAVVideoSettings, public ILAVVideoStatus,
                                                                           public ILAVVideoCallback, public IPropertyBag,
                                                                           public ILAVVideoSettingsMPCHCCustom
{
public:
  CLAVVideo(LPUNKNOWN pUnk, HRESULT* phr);
  ~CLAVVideo();

  // IUnknown
  DECLARE_IUNKNOWN;
  STDMETHODIMP NonDelegatingQueryInterface(REFIID riid, void** ppv);

  // ISpecifyPropertyPages2
  STDMETHODIMP GetPages(CAUUID *pPages);
  STDMETHODIMP CreatePage(const GUID& guid, IPropertyPage** ppPage);

  // ILAVVideoSettings
  STDMETHODIMP SetRuntimeConfig(BOOL bRuntimeConfig);
  STDMETHODIMP SetFormatConfiguration(LAVVideoCodec vCodec, BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetFormatConfiguration(LAVVideoCodec vCodec);
  STDMETHODIMP SetNumThreads(DWORD dwNum);
  STDMETHODIMP_(DWORD) GetNumThreads();
  STDMETHODIMP SetStreamAR(DWORD bStreamAR);
  STDMETHODIMP_(DWORD) GetStreamAR();
  STDMETHODIMP SetPixelFormat(LAVOutPixFmts pixFmt, BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetPixelFormat(LAVOutPixFmts pixFmt);
  STDMETHODIMP SetRGBOutputRange(DWORD dwRange);
  STDMETHODIMP_(DWORD) GetRGBOutputRange();

  STDMETHODIMP SetDeintFieldOrder(LAVDeintFieldOrder fieldOrder);
  STDMETHODIMP_(LAVDeintFieldOrder) GetDeintFieldOrder();
  STDMETHODIMP SetDeintForce(BOOL bForce);
  STDMETHODIMP_(BOOL) GetDeintForce();
  STDMETHODIMP SetDeintAggressive(BOOL bAggressive);
  STDMETHODIMP_(BOOL) GetDeintAggressive();

  STDMETHODIMP_(DWORD) CheckHWAccelSupport(LAVHWAccel hwAccel);
  STDMETHODIMP SetHWAccel(LAVHWAccel hwAccel);
  STDMETHODIMP_(LAVHWAccel) GetHWAccel();
  STDMETHODIMP SetHWAccelCodec(LAVVideoHWCodec hwAccelCodec, BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetHWAccelCodec(LAVVideoHWCodec hwAccelCodec);
  STDMETHODIMP SetHWAccelDeintMode(LAVHWDeintModes deintMode);
  STDMETHODIMP_(LAVHWDeintModes) GetHWAccelDeintMode();
  STDMETHODIMP SetHWAccelDeintOutput(LAVDeintOutput deintOutput);
  STDMETHODIMP_(LAVDeintOutput) GetHWAccelDeintOutput();
  STDMETHODIMP SetHWAccelDeintHQ(BOOL bHQ);
  STDMETHODIMP_(BOOL) GetHWAccelDeintHQ();
  STDMETHODIMP SetSWDeintMode(LAVSWDeintModes deintMode);
  STDMETHODIMP_(LAVSWDeintModes) GetSWDeintMode();
  STDMETHODIMP SetSWDeintOutput(LAVDeintOutput deintOutput);
  STDMETHODIMP_(LAVDeintOutput) GetSWDeintOutput();

  STDMETHODIMP SetDeintTreatAsProgressive(BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetDeintTreatAsProgressive();

  STDMETHODIMP SetDitherMode(LAVDitherMode ditherMode);
  STDMETHODIMP_(LAVDitherMode) GetDitherMode();

  STDMETHODIMP SetUseMSWMV9Decoder(BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetUseMSWMV9Decoder();

  STDMETHODIMP SetDVDVideoSupport(BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetDVDVideoSupport();

  STDMETHODIMP SetHWAccelResolutionFlags(DWORD dwResFlags);
  STDMETHODIMP_(DWORD) GetHWAccelResolutionFlags();

  STDMETHODIMP SetTrayIcon(BOOL bEnabled);
  STDMETHODIMP_(BOOL) GetTrayIcon();

  STDMETHODIMP SetDeinterlacingMode(LAVDeintMode deintMode);
  STDMETHODIMP_(LAVDeintMode) GetDeinterlacingMode();

  STDMETHODIMP SetGPUDeviceIndex(DWORD dwDevice);

  STDMETHODIMP_(DWORD) GetHWAccelNumDevices(LAVHWAccel hwAccel);
  STDMETHODIMP GetHWAccelDeviceInfo(LAVHWAccel hwAccel, DWORD dwIndex, BSTR *pstrDeviceName, DWORD *pdwDeviceIdentifier);

  STDMETHODIMP_(DWORD) GetHWAccelDeviceIndex(LAVHWAccel hwAccel, DWORD *pdwDeviceIdentifier);
  STDMETHODIMP SetHWAccelDeviceIndex(LAVHWAccel hwAccel, DWORD dwIndex, DWORD dwDeviceIdentifier);

  STDMETHODIMP SetH264MVCDecodingOverride(BOOL bEnabled);

  STDMETHODIMP SetEnableCCOutputPin(BOOL bEnabled);

  // ILAVVideoSettingsMPCHCCustom
  STDMETHODIMP SetPropertyPageCallback(HRESULT (*fpPropPageCallback)(IBaseFilter* pFilter));

  // ILAVVideoStatus
  STDMETHODIMP_(const WCHAR *) GetActiveDecoderName() { return m_Decoder.GetDecoderName(); }
  STDMETHODIMP GetHWAccelActiveDevice(BSTR *pstrDeviceName);

  // CTransformFilter
  STDMETHODIMP Stop();

  HRESULT CheckInputType(const CMediaType* mtIn);
  HRESULT CheckTransform(const CMediaType* mtIn, const CMediaType* mtOut);
  HRESULT DecideBufferSize(IMemAllocator * pAllocator, ALLOCATOR_PROPERTIES *pprop);
  HRESULT GetMediaType(int iPosition, CMediaType *pMediaType);

  HRESULT SetMediaType(PIN_DIRECTION dir, const CMediaType *pmt);
  HRESULT EndOfStream();
  HRESULT BeginFlush();
  HRESULT EndFlush();
  HRESULT NewSegment(REFERENCE_TIME tStart, REFERENCE_TIME tStop, double dRate);
  HRESULT Receive(IMediaSample *pIn);

  HRESULT CheckConnect(PIN_DIRECTION dir, IPin *pPin);
  HRESULT BreakConnect(PIN_DIRECTION dir);
  HRESULT CompleteConnect(PIN_DIRECTION dir, IPin *pReceivePin);

  HRESULT StartStreaming();

  int GetPinCount();
  CBasePin* GetPin(int n);

  STDMETHODIMP JoinFilterGraph(IFilterGraph * pGraph, LPCWSTR pName);

  // IPinSegmentEx
  HRESULT EndOfSegment();

  // ILAVVideoCallback
  STDMETHODIMP AllocateFrame(LAVFrame **ppFrame);
  STDMETHODIMP ReleaseFrame(LAVFrame **ppFrame);
  STDMETHODIMP Deliver(LAVFrame *pFrame);
  STDMETHODIMP_(LPWSTR) GetFileExtension();
  STDMETHODIMP_(BOOL) FilterInGraph(PIN_DIRECTION dir, const GUID &clsid) { if (dir == PINDIR_INPUT) return FilterInGraphSafe(m_pInput, clsid); else return FilterInGraphSafe(m_pOutput, clsid); }
  STDMETHODIMP_(DWORD) GetDecodeFlags() { return m_dwDecodeFlags; }
  STDMETHODIMP_(CMediaType&) GetInputMediaType() { return m_pInput->CurrentMediaType(); }
  STDMETHODIMP GetLAVPinInfo(LAVPinInfo &info) { if (m_LAVPinInfoValid) { info = m_LAVPinInfo; return S_OK; } return E_FAIL; }
  STDMETHODIMP_(CBasePin*) GetOutputPin() { return m_pOutput; }
  STDMETHODIMP_(CMediaType&) GetOutputMediaType() { return m_pOutput->CurrentMediaType(); }
  STDMETHODIMP DVDStripPacket(BYTE*& p, long& len) { static_cast<CDeCSSTransformInputPin*>(m_pInput)->StripPacket(p, len); return S_OK; }
  STDMETHODIMP_(LAVFrame*) GetFlushFrame();
  STDMETHODIMP ReleaseAllDXVAResources() { ReleaseLastSequenceFrame(); return S_OK; }
  STDMETHODIMP_(DWORD) GetGPUDeviceIndex() { return m_dwGPUDeviceIndex; }
  STDMETHODIMP_(BOOL) HasDynamicInputAllocator();
  STDMETHODIMP SetX264Build(int nBuild) { m_X264Build = nBuild; return S_OK; }
  STDMETHODIMP_(int) GetX264Build() { return m_X264Build; }

  // IPropertyBag
  STDMETHODIMP Read(LPCOLESTR pszPropName, VARIANT *pVar, IErrorLog *pErrorLog);
  STDMETHODIMP Write(LPCOLESTR pszPropName, VARIANT *pVar) { return E_NOTIMPL; }

  void DeliverFrame(FrameInfo *info);
  std::queue<ProcThread *> Q;

 public:
  // Pin Configuration
  const static AMOVIESETUP_MEDIATYPE    sudPinTypesIn[];
  const static UINT                     sudPinTypesInCount;
  const static AMOVIESETUP_MEDIATYPE    sudPinTypesOut[];
  const static UINT                     sudPinTypesOutCount;

private:
  HRESULT LoadDefaults();
  HRESULT ReadSettings(HKEY rootKey);
  HRESULT LoadSettings();
  HRESULT SaveSettings();

  HRESULT CreateTrayIcon();

  HRESULT CreateDecoder(const CMediaType *pmt);

  HRESULT GetDeliveryBuffer(IMediaSample** ppOut, int width, int height, AVRational ar, DXVA2_ExtendedFormat dxvaExtFormat, REFERENCE_TIME avgFrameDuration);
  HRESULT ReconnectOutput(int width, int height, AVRational ar, DXVA2_ExtendedFormat dxvaExtFlags, REFERENCE_TIME avgFrameDuration, BOOL bDXVA = FALSE);

  HRESULT SetFrameFlags(IMediaSample* pMS, LAVFrame *pFrame);

  HRESULT NegotiatePixelFormat(CMediaType &mt, int width, int height);
  BOOL IsInterlacedOutput();

  HRESULT CheckDirectMode();
  HRESULT DeDirectFrame(LAVFrame *pFrame, bool bDisableDirectMode = true);


  HRESULT Filter(LAVFrame *pFrame);
  HRESULT DeliverToRenderer(LAVFrame *pFrame);

  HRESULT PerformFlush();
  HRESULT ReleaseLastSequenceFrame();

  HRESULT GetD3DBuffer(LAVFrame *pFrame);
  HRESULT RedrawStillImage();
  HRESULT SetInDVDMenu(bool menu) { m_bInDVDMenu = menu; return S_OK; }

  BOOL ShouldDeint(LAVFrame *pFrame);
  BOOL ShouldScale();
  BOOL ShouldTonemap(LAVFrame *pFrame);
  BOOL HWPostProc();

 private:
  friend class CVideoInputPin;
  friend class CVideoOutputPin;
  friend class CDecodeManager;
  friend class CLAVSubtitleProvider;
  friend class CLAVSubtitleConsumer;

  CDecodeManager       m_Decoder;

  REFERENCE_TIME       m_rtPrevStart = 0;
  REFERENCE_TIME       m_rtPrevStop  = 0;
  REFERENCE_TIME       m_rtAvgTimePerFrame = AV_NOPTS_VALUE;

  BOOL                 m_bForceInputAR  = FALSE;
  BOOL                 m_bSendMediaType = FALSE;
  BOOL                 m_bFlushing      = FALSE;
  BOOL                 m_bForceFormatNegotiation = FALSE;

  HRESULT              m_hrDeliver      = S_OK;

  CLAVPixFmtConverter  m_PixFmtConverter;
  std::wstring         m_strExtension;

  BOOL                 m_bDXVAExtFormatSupport = TRUE;
  DWORD                m_bMadVR                = -1;
  DWORD                m_bOverlayMixer         = -1;
  DWORD                m_dwDecodeFlags         = 0;

  BOOL                 m_bInDVDMenu            = FALSE;

  LAVPixelFormat       m_filterPixFmt          = LAVPixFmt_None;
  int                  m_filterWidth           = 0;
  int                  m_filterHeight          = 0;
  LAVFrame             m_FilterPrevFrame;
  int                  m_scaleTarget           = 1280;

  BOOL                 m_LAVPinInfoValid       = FALSE;
  LAVPinInfo           m_LAVPinInfo;
  int                  m_X264Build             = -1;

  struct {
    AVMasteringDisplayMetadata Mastering;
    AVContentLightMetadata ContentLight;
  } m_SideData;

  CLAVVideoSubtitleInputPin *m_pSubtitleInput  = nullptr;
  CLAVSubtitleConsumer *m_SubtitleConsumer     = nullptr;

  CCCOutputPin         *m_pCCOutputPin         = nullptr;

  LAVFrame             *m_pLastSequenceFrame   = nullptr;

  AM_SimpleRateChange  m_DVDRate = AM_SimpleRateChange{AV_NOPTS_VALUE, 10000};

  BOOL                 m_bRuntimeConfig = FALSE;

  BOOL                 m_bFilterNeeded = TRUE;
 
  std::mutex deliver;

  ProcThread m_procThread[FILTER_PROC_THREADS];
  void ProcThreadDeInitAll() {
    for (int i = 0; i<FILTER_PROC_THREADS; i++) {
        if(m_procThread[i].Inited)
            m_procThread[i].DeInit();
    }
    for (int i = 0; i < FILTER_PROC_THREADS; i++) {
        if (m_procThread[i].Inited)
            m_procThread[i].WaitDeInitDone();
    }
  }

  int FeedFilter(FrameInfo *in);

  int ProcThreadInitAll(CLAVVideo *pClav, AVPixelFormat *pix_fmt, char *strFilterInit, char *strFilterConfig, BOOL bIsHardware) {
    for (int i = 0; i < FILTER_PROC_THREADS; i++) {
      m_procThread[i].Init(i, pClav, pix_fmt, strFilterInit, strFilterConfig, bIsHardware);
    }
    return 0;
  }

  struct VideoSettings {
    BOOL TrayIcon;
    DWORD StreamAR;
    DWORD NumThreads;
    BOOL bFormats[Codec_VideoNB];
    BOOL bMSWMV9DMO;
    BOOL bPixFmts[LAVOutPixFmt_NB];
    DWORD RGBRange;
    DWORD HWAccel;
    BOOL bHWFormats[HWCodec_NB];
    DWORD HWAccelResFlags;
    BOOL  HWAccelCUVIDXVA;
    DWORD HWDeintMode;
    DWORD HWDeintOutput;
    DWORD DeintFieldOrder;
    LAVDeintMode DeintMode;
    DWORD SWDeintMode;
    DWORD SWDeintOutput;
    DWORD DitherMode;
    BOOL bDVDVideo;
    DWORD HWAccelDeviceDXVA2;
    DWORD HWAccelDeviceDXVA2Desc;
    DWORD HWAccelDeviceD3D11;
    DWORD HWAccelDeviceD3D11Desc;
    BOOL bH264MVCOverride;
    BOOL bCCOutputPinEnabled;
  } m_settings;

  DWORD m_dwGPUDeviceIndex = DWORD_MAX;

  CBaseTrayIcon *m_pTrayIcon = nullptr;
  HRESULT (*m_fpPropPageCallback)(IBaseFilter* pFilter) = nullptr;

#ifdef DEBUG
  FloatingAverage<double> m_pixFmtTimingAvg;
#endif
};
