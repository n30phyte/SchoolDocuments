% MATLAB code for spectral analysis and lowpass filtering of an image
% see section 17.6, Fig. 17.20, Fig. 17.21
%
% reading the image 'ayantika.tif'
I = imread('ayantika.tif');
Iu=I;     % copy of original image
imshow(I)                   % display the image
fprintf('\nThe image Ayantika has been displayed')
fprintf('\nPress any key to continue')
pause
I = double(I);
I = I - mean(mean(I));
% 2D Bartlett window
x = bartlett(32);
for i = 1:32
    zx(i,:) = x';
	zy(:,i) = x ;
end
bartlett2D = zx .* zy;
%
n = 0;
% calculate power spectrum
P = zeros(256,256);
for (i = 1:16:320)
    for (j = 1:16:288)
        Isub = I(i:i+31,j:j+31).*bartlett2D;
        P = P + fftshift(fft2(Isub,256,256));
        n = n + 1;
    end
end
Pabs = (abs(P)/n).^2;
mesh([-128:127]*2/256,[-128:127]*2/256,Pabs/max(max(Pabs)));
xlabel('Horizontal Frequency'); ylabel('Vertical Frequency');
zlabel('Image Power Spectrum (in dB)');
print -dtiff plot1.tiff
fprintf('\nThe Image Power Spectrum has been displayed and saved')
fprintf('\nPress any key to continue')
pause


filter_coeff = [1  2 3 2 1; 2  3  4  3 2; 3  4  5  4 3; 2  3  4  3  2; 1  2  3  2 1]/65 ;
% Frequency Response plot
spec = fft2(filter_coeff,128,128) ;     % Frequency spectrum of 2-D Filter
R = abs(spec(1:65,1:65)) ;
mesh(R), grid
xlabel('Horizontal Frequency')
ylabel('Vertical Frequency');
zlabel('Frequency Response');
print -dtiff plot.tiff
fprintf('\nThe Frequency Response of the 2-D Filter has been displayed and saved')
fprintf('\nPress any key to continue')
pause
%
% Digital filtering the image
filtered_image = filter2(filter_coeff, double(I)) ;
imshow(uint8(filtered_image))
%imshow(Iu)
imwrite(uint8(filtered_image),'ayantika_filt.tif','tif') ;
fprintf('\nThe Filtered Image has been displayed and saved')


