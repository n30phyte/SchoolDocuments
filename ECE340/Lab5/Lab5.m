%% Question 1

% Assumed constants
Fs = 22050;
fc = 2500;

wc = fc / (Fs / 2);

window = hamming(513);

filter_coeff = fir1(513 - 1, wc, 'low', window);

figure;
freqz(filter_coeff, 1);

lovemono = audioread("love_mono22.wav");

love_low = filter(filter_coeff, 1, lovemono);

figure;
pwelch(lovemono, 500, 300, 500, Fs)

figure;
pwelch(love_low, window)

audiowrite("low_pass.wav", love_low, Fs)

%% Question 2
% Assumed constants
Fs = 22050;
fc = 5000;

wc = fc / (Fs / 2);

window = hamming(513);

filter_coeff = fir1(513 - 1, wc, 'high', window);

figure;
freqz(filter_coeff, 1);

love_high = filter(filter_coeff, 1, lovemono);

figure;
pwelch(love_high, window)

audiowrite("high_pass.wav", love_high, Fs)

%% Question 3

% 2.9 to 3.1 kHz

filter_coeff = fir1(513 - 1, [2900 / (Fs / 2), 3100 / (Fs / 2)], 'stop');

figure;
freqz(filter_coeff, 1);

love_band = filter(filter_coeff, 1, lovemono);

figure;
pwelch(love_band, 500, 300, 500, Fs)

audiowrite("band_stop.wav", love_band, Fs)
