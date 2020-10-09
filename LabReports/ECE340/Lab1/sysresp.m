function y = sysresp(x, a)
    %
    % computes the output in response to an arbitrary input x[n], n=0,бнN-1
    % assume that the system has 0 initial conditions
    % input:
    % x: the input signal,
    % a: the system parameter
    % output:
    % y: the output signal
    N = length(x); % Get length of input vector
    y = [0; N]; % Preallocate output

    for i = 1:N
        delay_val = 0; % Special case for y[0]

        if i - 1 > 0
            delay_val = y(i - 1);
        end

        y(i) = x(i) + a * delay_val;
    end
N
    % Plot output.
    figure;
    stem([0:N - 1], y); % Generate x values of [0, N)
    title_str = sprintf('a = %1.1f', a); % Add proper title
    title(title_str)% Print title
    xlabel('k')% Add labels
    ylabel('y')
end
