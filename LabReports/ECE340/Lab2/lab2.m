%% Question 1 Signal Convolution
k1 = [0:4];

% Part a
x1 = @(n) (n) .* ((0 <= n) & (n <= 4));
h1 = @(n) (2 - n) .* ((0 <= n) & (n <= 3));

% Part b
figure;
subplot(1, 2, 1);
stem(k1, x1(k1));
title('Plot of x[k] for 0\leq k\leq4');
xlabel('k');
ylabel('x[k]');

subplot(1, 2, 2);
stem(k1, h1(k1));
title('Plot of h[k] for 0 \leq k \leq 4');
xlabel('k');
ylabel('x[k]');

% Part C
y = conv(x1(k1), h1(k1));
figure;
stem([0:8], y);
title('Convolution of x[k] and h[k]');
xlabel('k');
ylabel('x[k] * h[k]');

%% Question 2 Audio Convolution
k = [0:50];

% Part a
h2 = @(n) (0.3 .* sinc(0.3 .* (n - 25)) .* (0.54 - (0.46 .* cos((2 .* pi .* n) ./ 50)))) .* ((0 <= n) & (n <= 50));

% Part b
figure;
stem(k, h2(k));
title('Filter impulse response');
xlabel('k');
ylabel('h[k]');

% Part c
[x3, fs] = audioread('baila.wav');
filt_x3 = conv(x3, h2(k));

% Part d
audiowrite('baila_filtered.wav', filt_x3, fs)

%% Question 3 Signal Aliasing
% Part a
n1 = [0:30]';
x1 = @(t) (cos(20 .* pi .* t));
x2 = @(t) (cos(180 .* pi .* t));

fs1 = 100;
T1 = 1 / fs1;

y1 = x1(T1 .* n1);
y2 = x2(T1 .* n1);

figure;
subplot(2, 1, 1);
stem(n1, y1);
title('Sampled signal y_1 result');
xlabel('Samples (n)');
ylabel('Amplitude (y_1)');

subplot(2, 1, 2);
stem(n1, y2);
title('Sampled signal y_2 result');
xlabel('Samples (n)');
ylabel('Amplitude (y_2)');

% Part b
n2 = [0:300];
fs2 = 1000;
T2 = 1 / fs2;
z1 = x1(T2 .* n2);
z2 = x2(T2 .* n2);

figure;
subplot(2, 1, 1);
plot(n2 / fs2, z1, 'r-', n1 / fs1, y1, 'b+');
xlabel('n');
ylabel('y_1[n] and z_1[n]');
legend('z_1[n]', 'y_1[n]');

subplot(2, 1, 2);
plot(n2 / fs2, z2, 'r-', n1 / fs1, y2, 'b+');
xlabel('n');
ylabel('y_2[n] and z_2[n]');
legend('z_2[n]', 'y_2[n]');

% Part c
x3 = @(t) (cos(380 .* pi .* t));
y3 = x3(T1 .* n1);

figure;
plot(n1, y1, 'r-', n1, y3, 'b+');
xlabel('n');
ylabel('y_1[n] and y_3[n]');
legend('y_1[n]', 'y_3[n]');

%% Question 4 Image Aliasing
% Part a
img = imread('barbaraLarge.jpg');

% Part b
figure;
imshow(img), colorbar;

% Part c
img09 = imresize(img, 0.9, 'nearest');
img07 = imresize(img, 0.7, 'nearest');
img05 = imresize(img, 0.5, 'nearest');

figure;
imshow(img09);
figure;
imshow(img07);
figure;
imshow(img05);