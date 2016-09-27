%TSNE dimensionality reduction and plotting clusters
% Load data
load 'mnist_train.mat'
% Set parameters
%run multiple for all csvs
filename = 'beartweetfeat.csv'; 
train_y = csvread('beartweetfeat.csv');
no_dims = 3;
initial_dims = 11;
perplexity = 30;
mappedX = tsne(train_y, [], no_dims, initial_dims, perplexity);

% Load data

vals = csvread('beartweetpc.csv')
ltext = ['Cluster 1'];
h = scatter3(mappedX(:,1), mappedX(:,2),mappedX(:,3) , (vals(:,2)*100),[(vals(:,6))]);
title('')
xlabel('Reduced Dimension 1')
ylabel('Reduced Dimension 2')
zlabel('Reduced Dimension 3')
hstruct = get(h);
hstruct.Children
legend(hstruct.Children,ltext);
%scatter3(vals(:,3), vals(:,4),vals(:,5) , (vals(:,2)*100),(vals(:,6)*55),'filled');
%text(vals(:,1), vals(:,2),labels)
%mappedX
