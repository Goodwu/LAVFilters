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

#include "stdafx.h"
#include "LAVVideo.h"
#include "Media.h"

#include <MMReg.h>
#include <Mfidl.h>

#include "moreuuids.h"

static void lav_free_lavframe(void *opaque, uint8_t *data)
{
  LAVFrame *frame = (LAVFrame *)opaque;
  FreeLAVFrameBuffers(frame);
  SAFE_CO_FREE(opaque);
}

static void lav_unref_frame(void *opaque, uint8_t *data)
{
  AVBufferRef *buf = (AVBufferRef *)opaque;
  av_buffer_unref(&buf);
}

static void avfilter_free_lav_buffer(LAVFrame *pFrame)
{
  av_frame_free((AVFrame **)&pFrame->priv_data);
}

AVPixelFormat ConvertFormat(LAVPixelFormat format)
{
  AVPixelFormat ff_pixfmt;

  switch (format)
  {
  case LAVPixFmt_YUV420:
    ff_pixfmt = AV_PIX_FMT_YUV420P;
    break;
  case LAVPixFmt_YUV422:
    ff_pixfmt = AV_PIX_FMT_YUV422P;
    break;
  case LAVPixFmt_P016:
    ff_pixfmt = AV_PIX_FMT_P010LE;
    break;
  default:
    ff_pixfmt = AV_PIX_FMT_NV12;
    break;
  }

  return ff_pixfmt;
}

LAVPixelFormat ConvertFormat(AVPixelFormat format)
{
  LAVPixelFormat pixfmt;

  switch (format)
  {
  case AV_PIX_FMT_YUV420P:
    pixfmt = LAVPixFmt_YUV420;
    break;
  case AV_PIX_FMT_YUV422P:
    pixfmt = LAVPixFmt_YUV422;
    break;
  case AV_PIX_FMT_P016LE:
  case AV_PIX_FMT_P010LE:
      pixfmt = LAVPixFmt_P016;
      break;
      default:
    pixfmt = LAVPixFmt_NV12;
    break;
  }

  return pixfmt;
}

void PrintFrameInfo(LAVFrame *pFrame)
{
  const char *pix_fmt_name = av_get_pix_fmt_name(ConvertFormat(pFrame->format));
  DbgLog((LOG_TRACE, 10, L"video_size=%dx%d:pix_fmt=%S:time_base=1/10000000:pixel_aspect=%d/%d", pFrame->width, pFrame->height, pix_fmt_name, pFrame->aspect_ratio.num, pFrame->aspect_ratio.den));
}

void PrintFrameInfo(AVFrame *pFrame)
{
  const char *pix_fmt_name = av_get_pix_fmt_name((AVPixelFormat)pFrame->format);
  DbgLog((LOG_TRACE, 10, L"video_size=%dx%d:pix_fmt=%S:time_base=1/10000000:pixel_aspect=%d/%d", pFrame->width, pFrame->height, pix_fmt_name, pFrame->sample_aspect_ratio.num, pFrame->sample_aspect_ratio.den));
}

void PrintExtFmt(DXVA2_ExtendedFormat ext)
{
  DbgLog((LOG_TRACE, 10, L"NominalRange:%d", ext.NominalRange));
  DbgLog((LOG_TRACE, 10, L"VideoChromaSubsampling:%d", ext.VideoChromaSubsampling));
  DbgLog((LOG_TRACE, 10, L"SampleFormat:%d", ext.SampleFormat));
  DbgLog((LOG_TRACE, 10, L"VideoLighting:%d", ext.VideoLighting));
  DbgLog((LOG_TRACE, 10, L"VideoPrimaries:%d", ext.VideoPrimaries));
  DbgLog((LOG_TRACE, 10, L"VideoTransferFunction:%d", ext.VideoTransferFunction));
  DbgLog((LOG_TRACE, 10, L"VideoTransferMatrix:%d", ext.VideoTransferMatrix));

  return;
}

void fillDXVAExtFormatPrint(DXVA2_ExtendedFormat &fmt, int range, int primaries, int matrix, int transfer, int chroma_sample_location, bool bClear)
{
  DbgLog((LOG_TRACE, 10, L"range:%d, primaries:%d, matrix:%d, transfer:%d, chroma_sample_location:%d", range, primaries, matrix, transfer, chroma_sample_location));
  return;
}

void fillExtProperties(AVFrame *out_frame, LAVFrame *frame)
{
  DXVA2_ExtendedFormat *ext = &frame->ext_format;

  switch (ext->NominalRange)
  {
  case DXVA2_NominalRange_0_255:
    out_frame->color_range = AVCOL_RANGE_JPEG;
    break;
  case DXVA2_NominalRange_16_235:
    out_frame->color_range = AVCOL_RANGE_MPEG;
    break;
  }

  switch (ext->VideoPrimaries)
  {
  case DXVA2_VideoPrimaries_BT709:
    out_frame->color_primaries = AVCOL_PRI_BT709;
    break;
  case DXVA2_VideoPrimaries_BT470_2_SysM:
    out_frame->color_primaries = AVCOL_PRI_BT470M;
    break;
  case DXVA2_VideoPrimaries_BT470_2_SysBG:
    out_frame->color_primaries = AVCOL_PRI_BT470BG;
    break;
  case DXVA2_VideoPrimaries_SMPTE170M:
    out_frame->color_primaries = AVCOL_PRI_SMPTE170M;
    break;
  case DXVA2_VideoPrimaries_SMPTE240M:
    out_frame->color_primaries = AVCOL_PRI_SMPTE240M;
    break;
  case 9:
    out_frame->color_primaries = AVCOL_PRI_BT2020;
    break;
  case 10:
    out_frame->color_primaries = AVCOL_PRI_SMPTE428;
    break;
  case 11:
    out_frame->color_primaries = AVCOL_PRI_SMPTE431;
    break;
  }

  switch (ext->VideoTransferMatrix)
  {
  case DXVA2_VideoTransferMatrix_BT709:
    out_frame->colorspace = AVCOL_SPC_BT709;
    break;
  case DXVA2_VideoTransferMatrix_BT601:
    out_frame->colorspace = AVCOL_SPC_BT470BG;
    break;
  case DXVA2_VideoTransferMatrix_SMPTE240M:
    out_frame->colorspace = AVCOL_SPC_SMPTE240M;
    break;
  case 4:
    out_frame->colorspace = AVCOL_SPC_BT2020_NCL;
    break;
  case 6:
    out_frame->colorspace = AVCOL_SPC_FCC;
    break;
  case 7:
    out_frame->colorspace = AVCOL_SPC_YCGCO;
    break;
  }

  switch (ext->VideoTransferFunction)
  {
  case DXVA2_VideoTransFunc_709:
    out_frame->color_trc = AVCOL_TRC_BT709;
    break;
  case DXVA2_VideoTransFunc_22:
    out_frame->color_trc = AVCOL_TRC_GAMMA22;
    break;
  case DXVA2_VideoTransFunc_28:
    out_frame->color_trc = AVCOL_TRC_GAMMA28;
    break;
  case DXVA2_VideoTransFunc_240M:
    out_frame->color_trc = AVCOL_TRC_SMPTE240M;
    break;
  case MFVideoTransFunc_Log_100:
    out_frame->color_trc = AVCOL_TRC_LOG;
    break;
  case MFVideoTransFunc_Log_316:
    out_frame->color_trc = AVCOL_TRC_LOG_SQRT;
    break;
  case 15:
    out_frame->color_trc = AVCOL_TRC_SMPTEST2084;
    break;
  case 16:
    out_frame->color_trc = AVCOL_TRC_ARIB_STD_B67;
    break;
  }

  switch (ext->VideoChromaSubsampling)
  {
  case DXVA2_VideoChromaSubsampling_MPEG2:
    out_frame->chroma_location = AVCHROMA_LOC_LEFT;
    break;
  case DXVA2_VideoChromaSubsampling_MPEG1:
    out_frame->chroma_location = AVCHROMA_LOC_CENTER;
    break;
  case DXVA2_VideoChromaSubsampling_Cosited:
    out_frame->chroma_location = AVCHROMA_LOC_TOPLEFT;
    break;
  }

  return;
}

BOOL CLAVVideo::ShouldDeint(LAVFrame *pFrame) {
  BOOL bFlush = pFrame->flags & LAV_FRAME_FLAG_FLUSH;
  if (m_Decoder.IsInterlaced(FALSE) && m_settings.DeintMode != DeintMode_Disable && (m_settings.SWDeintMode == SWDeintMode_YADIF || m_settings.SWDeintMode == SWDeintMode_W3FDIF_Simple || m_settings.SWDeintMode == SWDeintMode_W3FDIF_Complex) && ((bFlush && m_pFilterGraph) || pFrame->format == LAVPixFmt_YUV420 || pFrame->format == LAVPixFmt_YUV422 || pFrame->format == LAVPixFmt_NV12)) {
    return true;
  } else {
    return false;
  }
}

BOOL CLAVVideo::ShouldScale() {
  return true;
}

BOOL CLAVVideo::ShouldTonemap(LAVFrame *pFrame) {
  DbgLog((LOG_TRACE, 10, L"format:%d", pFrame->format));
  //PrintExtFmt(pFrame->ext_format);
  if (/*switch here && */ pFrame->format == LAVPixFmt_P016 || pFrame->ext_format.VideoTransferFunction == 15) {
    return true;
  } else {
    //PrintFrameInfo(pFrame);
    return false;
  }
}

BOOL CLAVVideo::HWPostProc()
{
  return true;
}

#define LOG_BUF_LEN 2048
inline void lavv_log_callback(void *ptr, int level, const char *fmt, va_list vl) {
  static char line[LOG_BUF_LEN] = {0};

  _vsnprintf_s(line, sizeof(line), fmt, vl);
  if (line[strlen(line) - 1] == '\n') {
    line[strlen(line) - 1] = 0;
  }

  DbgLog((LOG_TRACE, 10, L"%S", line));
}

HRESULT CLAVVideo::Filter(LAVFrame *pFrame) {
  int ret = 0;
  BOOL bFlush = pFrame->flags & LAV_FRAME_FLAG_FLUSH;
  if (pFrame->ext_format.VideoTransferFunction == 15) {
    pFrame->format = LAVPixFmt_P016;
  }
  av_log_set_callback(&lavv_log_callback);

  SYSTEMTIME t1, t2;
  GetSystemTime(&t1);

  if (ShouldDeint(pFrame) || ShouldScale() || ShouldTonemap(pFrame)) {
    AVPixelFormat ff_pixfmt = ConvertFormat(pFrame->format);
    if (!bFlush && (!m_pFilterGraph || pFrame->format != m_filterPixFmt || pFrame->width != m_filterWidth || pFrame->height != m_filterHeight)) {
      DbgLog((LOG_TRACE, 10, L":Filter()(init) Initializing post process filter for %S", av_get_pix_fmt_name(ff_pixfmt)));
      if (m_pFilterGraph) {
        avfilter_graph_free(&m_pFilterGraph);
        m_pFilterBufferSrc = nullptr;
        m_pFilterBufferSink = nullptr;
      }

      m_filterPixFmt = pFrame->format;
      m_filterWidth  = pFrame->width;
      m_filterHeight = pFrame->height;

      char args[512];
      enum AVPixelFormat pix_fmts[4];

      if (ff_pixfmt == AV_PIX_FMT_NV12) {
        pix_fmts[0] = AV_PIX_FMT_NV12;
        pix_fmts[1] = AV_PIX_FMT_YUV420P;
        pix_fmts[2] = AV_PIX_FMT_NONE;
      } else {
        pix_fmts[0] = ff_pixfmt;
        pix_fmts[1] = AV_PIX_FMT_NV12;
        pix_fmts[2] = AV_PIX_FMT_YUV420P;
        pix_fmts[3] = AV_PIX_FMT_NONE;
      }

      const AVFilter *buffersrc  = avfilter_get_by_name("buffer");
      const AVFilter *buffersink = avfilter_get_by_name("buffersink");
      AVFilterInOut *outputs = avfilter_inout_alloc();
      AVFilterInOut *inputs  = avfilter_inout_alloc();

      m_pFilterGraph = avfilter_graph_alloc();

      av_opt_set(m_pFilterGraph, "thread_type", "slice", AV_OPT_SEARCH_CHILDREN);
      av_opt_set_int(m_pFilterGraph, "threads", FFMAX(1, av_cpu_count() / 2), AV_OPT_SEARCH_CHILDREN);

      // 0/0 is not a valid value for avfilter, make sure it doesn't happen
      AVRational aspect_ratio = pFrame->aspect_ratio;
      if (aspect_ratio.num == 0 || aspect_ratio.den == 0)
        aspect_ratio = { 0, 1 };

      _snprintf_s(args, sizeof(args), "video_size=%dx%d:pix_fmt=%s:time_base=1/10000000:pixel_aspect=%d/%d", pFrame->width, pFrame->height, av_get_pix_fmt_name(ff_pixfmt), pFrame->aspect_ratio.num, pFrame->aspect_ratio.den);
      ret = avfilter_graph_create_filter(&m_pFilterBufferSrc, buffersrc, "in", args, nullptr, m_pFilterGraph);
      if (ret < 0) {
        DbgLog((LOG_TRACE, 10, L"::Filter()(init) Creating the input buffer filter failed with code %d", ret));
        avfilter_graph_free(&m_pFilterGraph);
        goto deliver;
      }

      ret = avfilter_graph_create_filter(&m_pFilterBufferSink, buffersink, "out", nullptr, nullptr, m_pFilterGraph);
      if (ret < 0) {
        DbgLog((LOG_TRACE, 10, L"::Filter()(init) Creating the buffer sink filter failed with code %d", ret));
        avfilter_free(m_pFilterBufferSrc);
        m_pFilterBufferSrc = nullptr;
        avfilter_graph_free(&m_pFilterGraph);
        goto deliver;
      }

      /* set allowed pixfmts on the output */
      av_opt_set_int_list(m_pFilterBufferSink->priv, "pix_fmts", pix_fmts, AV_PIX_FMT_NONE, 0);

      /* Endpoints for the filter graph. */
      outputs->name       = av_strdup("in");
      outputs->filter_ctx = m_pFilterBufferSrc;
      outputs->pad_idx    = 0;
      outputs->next       = nullptr;

      inputs->name       = av_strdup("out");
      inputs->filter_ctx = m_pFilterBufferSink;
      inputs->pad_idx    = 0;
      inputs->next       = nullptr;

      args[0] = 0;
      if(HWPostProc()) {
        /*
        if (ShouldDeint(pFrame)) {
          if (m_settings.SWDeintMode == SWDeintMode_YADIF)
            _snprintf_s(args, sizeof(args), "yadif=mode=%s:parity=auto:deint=interlaced", (m_settings.SWDeintOutput == DeintOutput_FramePerField) ? "send_field" : "send_frame");
          else if (m_settings.SWDeintMode == SWDeintMode_W3FDIF_Simple)
            _snprintf_s(args, sizeof(args), "w3fdif=filter=simple:deint=interlaced");
          else if (m_settings.SWDeintMode == SWDeintMode_W3FDIF_Complex)
            _snprintf_s(args, sizeof(args), "w3fdif=filter=complex:deint=interlaced");
          else
            ASSERT(0);
        }*/
        if (pFrame->width > m_scaleTarget &&ShouldScale()) {
          char scale_str[128];
          
          if (strlen(args))
            strcat_s(args, ",");
          else
            strcat_s(args, "hwupload_cuda,");
          _snprintf_s(scale_str, sizeof(scale_str), "scale_cuda=%d:-1,format=cuda", m_scaleTarget);
          strcat_s(args, scale_str);
        }
        if (ShouldTonemap(pFrame)) {
          if (strlen(args))
            strcat_s(args, ",hwdownload,format=p010le,hwupload,");
          else
            strcat_s(args, "hwupload,");
          strcat_s(args, "tonemap_opencl=t=bt2020:tonemap=hable:desat=0:format=nv12");
        }
        if (strlen(args)) {
          if (!ShouldTonemap(pFrame) && pFrame->format == LAVPixFmt_P016) {
            strcat_s(args, ",hwdownload,format=p010le");
          } else {
            strcat_s(args, ",hwdownload,format=nv12");
          }
        }

        //_snprintf_s(args, sizeof(args), "hwupload_cuda,scale_cuda=iw/2:-1,format=cuda,hwdownload,format=p010le,hwupload,tonemap_opencl=t=bt2020:tonemap=hable:desat=0:format=nv12,hwdownload,format=nv12");
        //_snprintf_s(args, sizeof(args), "hwupload_cuda,scale_cuda=720:-1,format=cuda,hwdownload,format=p010le");
      } else {
        if (ShouldDeint(pFrame)) {
          if (m_settings.SWDeintMode == SWDeintMode_YADIF)
            _snprintf_s(args, sizeof(args), "yadif=mode=%s:parity=auto:deint=interlaced", (m_settings.SWDeintOutput == DeintOutput_FramePerField) ? "send_field" : "send_frame");
          else if (m_settings.SWDeintMode == SWDeintMode_W3FDIF_Simple)
            _snprintf_s(args, sizeof(args), "w3fdif=filter=simple:deint=interlaced");
          else if (m_settings.SWDeintMode == SWDeintMode_W3FDIF_Complex)
            _snprintf_s(args, sizeof(args), "w3fdif=filter=complex:deint=interlaced");
          else
            ASSERT(0);
        }
        if (pFrame->width > m_scaleTarget && ShouldScale()) {
          char scale_str[128];
          if (strlen(args))
            strcat_s(args, ",");
          _snprintf_s(scale_str, sizeof(scale_str), "scale=%d:-1:flags=neighbor", m_scaleTarget);
          strcat_s(args, scale_str);
        }
        if (ShouldTonemap(pFrame)) {
          if (strlen(args))
            strcat_s(args, ",");
          strcat_s(args, "zscale=t=linear:npl=250,tonemap=tonemap=clip:param=1.0:desat=0,zscale=t=bt709,format=yuv420p");
        }
      }

      if (!strlen(args)) {
        //FIXME: This should not exist, just make the filter work
        _snprintf_s(args, sizeof(args), "scale=iw:-1:flags=neighbor");
      }
      DbgLog((LOG_TRACE, 10, L"filter:%S", args));
      if ((ret = avfilter_graph_parse_ptr(m_pFilterGraph, args, &inputs, &outputs, nullptr)) < 0) {
        DbgLog((LOG_TRACE, 10, L"::Filter()(init) Parsing the graph failed with code %d", ret));
        avfilter_graph_free(&m_pFilterGraph);
        goto deliver;
      }

      //init hardware
      if (HWPostProc()) {
        //TODO: Check if device will be freed in avfilter_graph_free
        AVBufferRef *device;
        ret = av_hwdevice_ctx_create(&device, AV_HWDEVICE_TYPE_OPENCL, NULL, NULL, 0);
        if (ret < 0)
          goto deliver;
        for (unsigned int i = 0; i < m_pFilterGraph->nb_filters; i++) {
          m_pFilterGraph->filters[i]->hw_device_ctx = av_buffer_ref(device);
          if (!m_pFilterGraph->filters[i]->hw_device_ctx) {
            ret = AVERROR(ENOMEM);
            goto deliver;
          }
        }
        //TODO: Check if here is needed
        //av_buffer_unref(&device);
      }

      if ((ret = avfilter_graph_config(m_pFilterGraph, nullptr)) < 0) {
        DbgLog((LOG_TRACE, 10, L"::Filter()(init) Configuring the graph failed with code %d", ret));
        avfilter_graph_free(&m_pFilterGraph);
        goto deliver;
      }

      DbgLog((LOG_TRACE, 10, L":Filter()(init) avfilter Initialization complete"));
    }

    GetSystemTime(&t2);
    DbgLog((LOG_TRACE, 10, L"After init: %u ms", ((t2.wMinute - t1.wMinute) * 60 + t2.wSecond - t1.wSecond) * 1000 + t2.wMilliseconds - t1.wMilliseconds));

    if (!m_pFilterGraph)
      goto deliver;

    if (pFrame->direct) {
      HRESULT hr = DeDirectFrame(pFrame, true);
      if (FAILED(hr)) {
        ReleaseFrame(&pFrame);
        return hr;
      }
    }

    AVFrame *in_frame = nullptr;
    BOOL refcountedFrame = (m_Decoder.HasThreadSafeBuffers() == S_OK);
    // When flushing, we feed a NULL frame
    if (!bFlush) {
      in_frame = av_frame_alloc();

      for (int i = 0; i < 4; i++) {
        in_frame->data[i] = pFrame->data[i];
        in_frame->linesize[i] = (int)pFrame->stride[i];
      }

      in_frame->width               = pFrame->width;
      in_frame->height              = pFrame->height;
      in_frame->format              = ff_pixfmt;
      in_frame->pts                 = pFrame->rtStart;
      in_frame->interlaced_frame    = pFrame->interlaced;
      in_frame->top_field_first     = pFrame->tff;
      in_frame->sample_aspect_ratio = pFrame->aspect_ratio;
      fillExtProperties(in_frame, pFrame);

      if (refcountedFrame) {
        AVBufferRef *pFrameBuf = av_buffer_create(nullptr, 0, lav_free_lavframe, pFrame, 0);
        const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get((AVPixelFormat)in_frame->format);
        int planes = (in_frame->format == AV_PIX_FMT_NV12) ? 2 : desc->nb_components;

        for (int i = 0; i < planes; i++) {
          int h_shift    = (i == 1 || i == 2) ? desc->log2_chroma_h : 0;
          int plane_size = (in_frame->height >> h_shift) * in_frame->linesize[i];

          AVBufferRef *planeRef = av_buffer_ref(pFrameBuf);
          in_frame->buf[i] = av_buffer_create(in_frame->data[i], plane_size, lav_unref_frame, planeRef, AV_BUFFER_FLAG_READONLY);
        }
        //FIXME: I don't think this unref is needed here
        //av_buffer_unref(&pFrameBuf);
      }

      m_FilterPrevFrame = *pFrame;
      memset(m_FilterPrevFrame.data, 0, sizeof(m_FilterPrevFrame.data));
      m_FilterPrevFrame.destruct = nullptr;
    } else {
      if (!m_FilterPrevFrame.height) // if height is not set, the frame is most likely not valid
        return S_OK;
      *pFrame = m_FilterPrevFrame;
    }

    GetSystemTime(&t2);
    DbgLog((LOG_TRACE, 10, L"Before write frame: %u ms", ((t2.wMinute - t1.wMinute) * 60 + t2.wSecond - t1.wSecond) * 1000 + t2.wMilliseconds - t1.wMilliseconds));
    if ((ret = av_buffersrc_write_frame(m_pFilterBufferSrc, in_frame)) < 0) {
      av_frame_free(&in_frame);
      goto deliver;
    }

    BOOL bFramePerField = (m_settings.SWDeintMode == SWDeintMode_YADIF && m_settings.SWDeintOutput == DeintOutput_FramePerField)
                        || m_settings.SWDeintMode == SWDeintMode_W3FDIF_Simple || m_settings.SWDeintMode == SWDeintMode_W3FDIF_Complex;

    AVFrame *out_frame = av_frame_alloc();
    HRESULT hrDeliver = S_OK;
    GetSystemTime(&t2);
    DbgLog((LOG_TRACE, 10, L"Before get frame: %u ms", ((t2.wMinute - t1.wMinute) * 60 + t2.wSecond - t1.wSecond) * 1000 + t2.wMilliseconds - t1.wMilliseconds));
    while (SUCCEEDED(hrDeliver) && (av_buffersink_get_frame(m_pFilterBufferSink, out_frame) >= 0)) {
      LAVFrame *outFrame = nullptr;
      AllocateFrame(&outFrame);

      REFERENCE_TIME rtDuration = pFrame->rtStop - pFrame->rtStart;
      if (bFramePerField)
        rtDuration >>= 1;

      const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get((AVPixelFormat)out_frame->format);
      // Copy most settings over
      outFrame->format       = ConvertFormat((AVPixelFormat)out_frame->format);
      outFrame->bpp          = av_get_bits_per_pixel(desc);
      fillDXVAExtFormat(outFrame->ext_format, out_frame->color_range - 1, out_frame->color_primaries, out_frame->colorspace, out_frame->color_trc, out_frame->chroma_location, false);
      if (outFrame->ext_format.VideoTransferFunction == DXVA2_VideoTransFunc_709) {
        outFrame->ext_format.VideoPrimaries = DXVA2_VideoPrimaries_BT709;
        outFrame->ext_format.VideoTransferMatrix = DXVA2_VideoTransferMatrix_BT709;
      }
      outFrame->avgFrameDuration = pFrame->avgFrameDuration;
      outFrame->flags        = pFrame->flags;

      outFrame->width        = out_frame->width;
      outFrame->height       = out_frame->height;
      outFrame->aspect_ratio = out_frame->sample_aspect_ratio;
      outFrame->tff          = out_frame->top_field_first;

      REFERENCE_TIME pts     = av_rescale(out_frame->pts, m_pFilterBufferSink->inputs[0]->time_base.num * 10000000LL, m_pFilterBufferSink->inputs[0]->time_base.den);
      outFrame->rtStart      = pts;
      outFrame->rtStop       = pts + rtDuration;

      if (bFramePerField) {
        if (outFrame->avgFrameDuration != AV_NOPTS_VALUE)
          outFrame->avgFrameDuration /= 2;
      }

      for (int i = 0; i < 4; i++) {
        outFrame->data[i] = out_frame->data[i];
        outFrame->stride[i] = out_frame->linesize[i];
      }

      outFrame->destruct = avfilter_free_lav_buffer;
      outFrame->priv_data = av_frame_alloc();
      av_frame_move_ref((AVFrame *)outFrame->priv_data, out_frame);

      GetSystemTime(&t2);
      DbgLog((LOG_TRACE, 10, L"Before deliver: %u ms", ((t2.wMinute - t1.wMinute) * 60 + t2.wSecond - t1.wSecond) * 1000 + t2.wMilliseconds - t1.wMilliseconds));
      hrDeliver = DeliverToRenderer(outFrame);
    }
    if (!refcountedFrame)
      ReleaseFrame(&pFrame);
    av_frame_free(&in_frame);
    av_frame_free(&out_frame);

    // We EOF'ed the graph, need to close it
    if (bFlush) {
      if (m_pFilterGraph) {
        avfilter_graph_free(&m_pFilterGraph);
        m_pFilterBufferSrc = nullptr;
        m_pFilterBufferSink = nullptr;
      }
    }

    return S_OK;
  } else {
    m_filterPixFmt = LAVPixFmt_None;
  deliver:
    return DeliverToRenderer(pFrame);
  }
}
