% Q1
k1 = [-10:40];
k2 = [0:100];

x1 = -5.1 * sin(0.1 * pi * k1 - (3 * pi) / 4) + 1.1 * cos(0.4 * pi * k1);
x2 = ((-0.9).^k2) .* exp((j * pi .* k2) / 10);

% Plot lines
figure
% Plot x_1 in top two slots
subplot(3, 1, 1);
stem(k1, x1);
title('Plot of x_1 vs k')
xlabel('k')
ylabel('x_1')
set(gca,'xMinorTick','on')
set(gca,'yMinorTick','on')
hold on
% plot real(x_2)
subplot(3, 1, 2);
stem(k2, real(x2))
title('Plot of x_2 vs k (real)')
xlabel('k')
ylabel('x_2 real')
set(gca,'xMinorTick','on')
set(gca,'yMinorTick','on')
% plot im(x_2)
subplot(3, 1, 3);
stem(k2, imag(x2));
title('Plot of x_2 vs k (imaginary)')
xlabel('k')
ylabel('x_2 imaginary')
set(gca,'xMinorTick','on')
set(gca,'yMinorTick','on')

% Select first plot to draw the lines
subplot(3, 1, 1);
% Generate lines for period
for i = [-2:20:40]
    xline(i);
end

hold off

% Calculate energy
E1 = sum(x1.^2)
E2 = sum(abs(x2).^2)

% Q2
figure
[x3, f] = audioread('baila.wav');
plot(x3);
size(x3)
ticks = [0:f:length(x3)];
title('baila waveform')
xticks(ticks)
xticklabels([0:length(ticks)])
xlabel('Seconds')
ylabel('Signal value')

% Calculate energy with same formula as question 1
E3 = sum(x3.^2)

% Take first half
x3s = x3(1:length(x3) / 2);
audiowrite('baila_half.wav', x3s, f);

% Q3
lena = imread('lena.jpg');
size(lena)
max(max(lena))
lena_bright = lena + 30;
imwrite(lena_bright, 'lena_bright.jpg', 'jpg', 'Quality', 100);

%Q4
x = zeros(1,50) + 1; % Generate [1 1 ... 1]
sysresp(x, 0.5);
sysresp(x, 2);

sysresp(x, 0.1);
