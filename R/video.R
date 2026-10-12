#' Generate video showing spectrogram
#'
#' @param wav wav file
#' @param out output mp4 file
#' @param start start within recording in seconds. defaults to 0
#' @param dur stop within recording in seconds. defaults to NULL
#' @param fps frames per second. defaults to 25
#' @param window_s visible window length in seconds. defaults to 10
#' @param fmax max. Frequency in kHz. defaults to 10
#' @param wl FFT-window. defaults to 1024
#' @param ovlp overlap
#' @param dyn_range dynamic range in dB
#' @param width width
#' @param height height
#' @param pal color palette
#' @param ffmpeg ffmpeg
#' @param cores number of cores
#' @param keep_frames logical
#'
#' @import tuneR
#' @import seewave
#' @importFrom graphics abline axis box mtext par
#' @importFrom grDevices dev.off png colorRampPalette
#'
#'
#' @export
#'
wav2video <- function(
    wav,
    out        = sub("\\.wav$", ".mp4", wav, ignore.case = TRUE),
    start      = 0,
    dur        = NULL,
    fps        = 25,
    window_s   = 10,
    fmax       = 10,
    wl         = 2048,
    ovlp       = 75,
    dyn_range  = 40,
    width      = 1280,
    height     = 540,
    pal        = colorRampPalette(c("#000000", "#000123", "#000537",
                                               "#051586", "#0e9276", "#1dd752", "#1afe36"),
                                             bias = 1.6)(256),
    ffmpeg     = "ffmpeg",
    keep_frames = FALSE,
    cores = 1) {

  ## Checks --------------------------------------------------------------------
  stopifnot(file.exists(wav))
  if (!nzchar(Sys.which(ffmpeg)) && !file.exists(ffmpeg)) {
    stop("ffmpeg not found. Specify ffmpeg in function call or add to PATH")
  }

  ## Read Audio ----------------------------------------------------------------
  wav <- tools::file_path_as_absolute(wav)
  w <- tuneR::readWave(wav, from = start,
                       to = if (is.null(dur)) Inf else start + dur,
                       units = "seconds")
  if (w@stereo) w <- mono(w, "left")
  total <- length(w@left) / w@samp.rate

  ## Compute spektrogram -------------------------------------------------------
  sp   <- spectro(w, wl = wl, ovlp = ovlp, plot = FALSE, dB = "max0")
  keep <- sp$freq <= fmax
  fr   <- sp$freq[keep]
  tt   <- sp$time
  z    <- (pmax(sp$amp[keep, , drop = FALSE], -dyn_range) + dyn_range) / dyn_range
  rm(sp)

  ## rendre Frames -------------------------------------------------------------
  tmp <- tempfile("frames_"); dir.create(tmp)
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else png
  n_frames <- ceiling(total * fps)

  render <- function(i) {
    tp <- (i - 1) / fps
    t0 <- tp - window_s / 2
    t1 <- tp + window_s / 2
    cols <- which(tt >= t0 & tt <= t1)

    dev(file.path(tmp, sprintf("f%06d.png", i)), width, height)

    par(mar = c(3, 3.5, 1, 1), bg = "black", fg = "gray75",
        col.axis = "white", col.lab = "white")

    plot(NA, xlim = c(t0, t1) + start, ylim = c(0, fmax),
         xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")

    if (length(cols) > 1) {
      image(tt[cols] + start, fr, t(z[, cols, drop = FALSE]),
            zlim = c(0, 1), col = pal, add = TRUE, useRaster = TRUE)
    }

    axis(1, col = "gray75", col.ticks = "gray75", col.axis = "white")
    axis(2, at = seq(0, fmax, 3), col = "gray75", col.ticks = "gray75",
         col.axis = "white", las = 1)
    mtext("kHz", side = 3, adj = 0, line = -0.5, col = "white", cex = 0.8)
    abline(v = tp + start, col = "white", lwd = 1.5)
    box(col = "white")
    dev.off()
    invisible(NULL)
  }

  message("Render ", n_frames, " Frames ...")
  invisible(parallel::mclapply(seq_len(n_frames), render, mc.cores = cores))

  ## Combine video + Audio with ffmpeg -----------------------------------------
  args <- c("-y",
            "-framerate", fps, "-i", shQuote(file.path(tmp, "f%06d.png")),
            "-ss", start, "-t", total, "-i", shQuote(wav),
            "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", 20,
            "-c:a", "aac", "-strict", "-2", "-b:a", "192k", "-shortest",
            shQuote(out))
  status <- system2(ffmpeg, args)
  if (status != 0) stop("ffmpeg failed (Status ", status, ")")

  if (!keep_frames) unlink(tmp, recursive = TRUE) else message("Frames: ", tmp)
  message("done: ", out)
  invisible(out)
}
