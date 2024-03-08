## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries-and-paths------------------------------------------------------
# Install missing libraries and load all libraries
libraries = c('seewave', 'tuneR', 'callsync', 'stringr', 'parallel')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Paths to all files
path_recordings = 'audio'
path_chunks = 'results/chunks'
path_calls = 'results/calls'
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/vignettes/audio'
file_1 = '/artificial_recording_ID_ind1_ID_REC_rec1_REC.wav'
file_2 = '/artificial_recording_drifted_ID_ind2_ID_REC_rec1_REC.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste0(path_recordings, file_1)
local_file_2 = paste0(path_recordings, file_2)

# Download audio files from GitHub
options(timeout = 600)
if(!dir.exists('audio')) dir.create('audio')
if(!file.exists(local_file_1)) 
  download.file(url_1, destfile = local_file_1, mode = 'wb')
if(!file.exists(local_file_2)) 
  download.file(url_2, destfile = local_file_2, mode = 'wb')

# Clean and create results directories
if(dir.exists('results')) unlink('results', recursive = TRUE)
dir.create(path_chunks, recursive = TRUE)
dir.create(path_calls, recursive = TRUE)

## ----major-alignment----------------------------------------------------------
align(chunk_size = 2,
      step_size = 0.5,
      path_recordings = path_recordings,
      path_chunks = path_chunks, 
      keys_id = c('ID_', '_ID'),
      keys_rec = c('REC_', '_REC'),
      blank = 0.5, 
      wing = 0.5, 
      save_pdf = TRUE, 
      save_log = TRUE)

## ----call-detection-and-assignment--------------------------------------------
detect.and.assign(path_chunks = path_chunks,
                  path_calls = path_calls,
                  ffilter_from = 1000,
                  threshold = 0.2, 
                  msmooth = c(500, 95), 
                  min_dur = 0.05, 
                  max_dur = 0.5,
                  step_size = 0.02, 
                  wing = 10) 

## ----trace-fundamental-frequency, fig.cap = "Example of a fundamental frequency trace. Black dashed lines are detected start and end times. Green dashed line is the detected fundamental frequency."----
# Audio files 
calls = list.files(path_calls,  '*wav', full.names = T)

# Load waves
waves = lapply(calls, load.wave, ffilter_from = 700)
names(waves) = basename(calls)

# Detect call
message('Starting detections.')
detections = lapply(waves, call.detect, 
                    threshold = 0.3,
                    msmooth = c(500, 95))

# Extract one call per wave
new_waves = lapply(1:length(waves), function(i) 
  waves[[i]][detections[[i]]$start:detections[[i]]$end])

# Trace fundamental
message('Tracing.')
traces = mclapply(new_waves, function(new_wave)
  trace.fund(new_wave, spar = 0.3, freq_lim = c(0.5, 2), 
             thr = 0.15, hop = 5, noise_factor = 1.5), 
  mc.cores = 1)
names(traces) = basename(calls)

# Plot example of trace
better.spectro(waves[[1]])
abline(v = detections[[1]][c('start', 'end')]/waves[[1]]@samp.rate,
       lty = 2, lwd = 3)
lines(traces[[1]]$time + detections[[1]]$start/waves[[1]]@samp.rate,
      traces[[1]]$fund,
      lty = 2, col = 3, lwd = 3)

## ----analysis, fig.cap = "Scatter plot of meand fundamental frequency [Hz] vs duration [s]. Dots are individual calls and are coloured by individual."----
# Take measurements
measurements = measure.trace.multiple(traces = traces, 
                                      new_waves = new_waves, 
                                      waves = waves, 
                                      detections = detections)

# Plot
individuals = traces |> names() |> strsplit('@') |> sapply(`[`, 2) |> 
  as.factor() |> as.integer()
colours = c('#d11141', '#00aedb')[individuals]
plot(measurements$mean_fund_hz, 
     measurements$duration_s, 
     col = colours, pch = 16,
     xlab = 'Mean fund freq [Hz]',
     ylab = 'Duration [s]')

