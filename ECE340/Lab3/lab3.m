%% Question 1

% Part a
n1 = [0:10];
h = @(n) (n .* ((0.5).^n) .* sin((pi.*n)./6)) .* (n >= 0);

h1 = h(n1);
figure;
stem(n1, h1);
title("h_1");
legend('h_1[n]');
ylabel("h response");
xlabel("n samples");

% Part c
Nz = [0 4 0 -1];
Dz = [16 -16*sqrt(3) 20 -4*sqrt(3) 1];

[Nz, Dz] = eqtflength(Nz,Dz);

n2 = zeros(1, 11);
n2(1) = 1;
h2 = filter(Nz, Dz, n2);

% Part d
figure;
hold on
stem(n1, h1);
stem(n1, h2);
title("h_1 and h_2 over n");
legend('h_1[n]', 'h_2[n]');
ylabel("h response");
xlabel("n samples");
hold off

%% Question 2

% Part a
N1 = [2 2];
D1 = [1 -1.25];

N2 = [2 2];
D2 = [1 -0.8];

[z1, p1, ~] = tf2zpk(N1, D1);
[z2, p2, ~] = tf2zpk(N2, D2);

figure;
zplane(z1, p1);
title("Pole-zero plot for H_{1}(z)");
xlabel("Real");
xlabel("Imaginary");

figure;
zplane(z2, p2);
title("Pole-zero plot for H_{2}(z)");
xlabel("Real");
xlabel("Imaginary");

% Part b
syms z n

w = linspace(0, 2*pi, 200);
H1(z) = (2 + 2 * z^(-1))/(1 - 1.25 * z^(-1));
H2(z) = (2 + 2 * z^(-1))/(1 - 0.8 * z^(-1));

H1 = H1(cos(w) + j*sin(w));
H2 = H2(cos(w) + j*sin(w));

figure;
subplot(1,2,1);
plot(w, abs(H1));
title("|H_1(e^{j\omega})|");
xlabel("\omega");
ylabel("Magnitude");
subplot(1,2,2);
plot(w, angle(H1));
title("\angleH_1(e^{j\omega})");
xlabel("\omega");
ylabel("Phase");

figure;
subplot(1,2,1);
plot(w, abs(H2));
title("|H_2(e^{j\omega})|");
xlabel("\omega");
ylabel("Magnitude");
subplot(1,2,2);
plot(w, angle(H2));
title("\angleH_2(e^{j\omega})");
xlabel("\omega");
ylabel("Phase");

% Part c
h1 = matlabFunction(2 * (1.25^n * heaviside(n)) + 2 * (1.25^(n-1) * heaviside(n-1)));
h2 = matlabFunction(2 * (0.8^n * heaviside(n)) + 2 * (0.8^(n-1) * heaviside(n-1)));

figure;
stem([0:25], h1([0:25]));
title("Impulse response of h_1[n]");
xlabel("n");
ylabel("h_1[n]");

figure;
stem([0:25], h2([0:25]));
title("Impulse response of h_2[n]");
xlabel("n");
ylabel("h_2[n]");