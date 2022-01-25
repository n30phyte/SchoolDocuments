%% Q1

[ubw, Fs] = audioread('ubw.wav');
[samples, ~] = size(ubw);

samples = samples - 1;

disp(samples/Fs)

bps = audioinfo('ubw.wav').BitsPerSample;

disp(bps)

bitrate = bps * Fs * audioinfo('ubw.wav').NumChannels;

disp(bitrate)

%% Q2

X = fft(ubw);

X_scaled = abs(X/sqrt(samples));
X_primedb = 20 * log10(X_scaled(1:samples/2 + 1,1));

fm = (Fs/1000)*(0:(samples/2))/samples;

figure;
plot(fm, X_primedb);
xlabel('Frequency (kHz)')
ylabel("X'[r] (dB)")
xlim([0 23])


%% Q3

[x,Fs] = audioread('love_mono22.wav');
[pxx,f] = pwelch(x,500,300,500,Fs);
figure;
plot(f/1000,10*log10(pxx));
xlabel('Frequency (kHz)')
ylabel('PSD (dB/Hz)')


[x,Fs] = audioread('ubw.wav');
[pxx,f] = pwelch(x,500,300,500,Fs);
figure;
plot(f/1000,10*log10(pxx));
xlabel('Frequency (kHz)')
ylabel('PSD (dB/Hz)')