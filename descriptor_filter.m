clc
%%
% Prompt user to select a CSV file
[filename, pathname] = uigetfile({'*.csv'}, 'Select CSV File');
if isequal(filename, 0)
    error('No file selected. Exiting script.');
end

% Read the CSV file
data_table = readtable(fullfile(pathname, filename), 'PreserveVariableNames', true);

% Convert table to numeric matrix while keeping variable names
data = table2array(data_table);  % Extract numerical values
var_names = data_table.Properties.VariableNames;  % Save original variable names

% Ensure first row contains variable indices (0 to N)
var_indices = 1:size(data, 2);  % Start from 1 (MATLAB index)

%% Step 1: Remove near-constant features (≥80% identical values)
Y = zeros(1, size(data, 2));  % Preallocate
for i = 2:size(data, 2)
    Y(i) = max(histcounts(data(:, i)));  % Count most frequent value
end

idx_const = find(Y(:) >= 0.8 * (size(data, 1)));  % Find constant-like features
data_nonconst = data;
data_nonconst(:, idx_const) = [];  % Remove them
var_indices(idx_const) = [];  

%% Step 2: Remove multicollinear features (correlation > 0.95)
data_refine = data_nonconst;
var_indices_refine = var_indices;

% Compute correlation matrix
cor = abs(corrcoef(data_refine(:, 2:end)));  % Ignore response variable in correlation
[a, b] = find(cor > 0.95);  
A = [b, a];
A(b >= a, :) = [];  % Remove duplicate pairs

% Find most correlated features and remove iteratively
while ~isempty(A)
    % Count occurrences of each feature in correlations
    unique_features = unique(A(:, 1));
    num = arrayfun(@(x) sum(A(:, 1) == x), unique_features);

    % Find most frequently occurring correlated feature
    [~, id_max] = max(num);
    feature_to_remove = unique_features(id_max);

    % Remove the feature from dataset
    data_refine(:, feature_to_remove + 1) = [];
    var_indices_refine(feature_to_remove + 1) = [];

    % Recompute correlation after removal
    cor = abs(corrcoef(data_refine(:, 2:end)));  
    [a, b] = find(cor > 0.95);
    A = [b, a];
    A(b >= a, :) = [];
end

%% Step 3: Remove weaker response-correlated feature in correlated pairs
while ~isempty(A)
    cor1 = abs(corr(data_refine(:, 1), data_refine(:, A(1, 1) + 1))); 
    cor2 = abs(corr(data_refine(:, 1), data_refine(:, A(1, 2) + 1))); 

    % Remove feature with lower correlation to response variable
    if cor1 >= cor2
        data_refine(:, A(1, 2) + 1) = [];
        var_indices_refine(A(1, 2) + 1) = [];
    else
        data_refine(:, A(1, 1) + 1) = [];
        var_indices_refine(A(1, 1) + 1) = [];
    end

    % Recompute correlation matrix
    cor = abs(corrcoef(data_refine(:, 2:end)));
    [a, b] = find(cor > 0.95);
    A = [b, a];
    A(b >= a, :) = [];
end

%% Step 4: Ensure Variable Names Match the Refined Dataset
selected_variable_names = var_names(var_indices_refine);  % Keep only selected variable names
if length(selected_variable_names) ~= size(data_refine, 2)
    error('Mismatch between variable names and refined dataset columns.');
end

%% Save the refined dataset
save('KDdata_refine.mat', 'data_refine', 'selected_variable_names');

% Save refined dataset as CSV
refined_table = array2table(data_refine, 'VariableNames', selected_variable_names);
writetable(refined_table, 'KDdata_refine2.csv');

disp('Refined dataset saved as KDdata_refine.mat and KDdata_refine.csv.');
