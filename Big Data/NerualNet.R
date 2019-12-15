setwd("E:/SFU/2019Fall/STAT440/Assignment3")

library(keras)
# install_keras()

mnist <- dataset_fashion_mnist()

train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

#Scaling
train_images <- train_images / 255
test_images <- test_images / 255

# train_images[1,,]

model <- keras_model_sequential()

#Flatten 28*28 pixels into 784 pixels
model %>% layer_flatten(input_shape = c(28, 28)) %>%
    #First hidden layer with 128 neurons
    layer_dense(units = 128, activation = 'relu') %>%
    #Output layer
    layer_dense(units = 10, activation = 'softmax')

model %>% compile(
    optimizer = 'adam', 
    loss = 'sparse_categorical_crossentropy',
    metrics = c('accuracy')
)

model %>% fit(train_images, train_labels, epochs = 5)

#a)
str(mnist)

#b)
#The hidden dense layer contains 128 neurons

#c)
score <- model %>% evaluate(test_images, test_labels)

print(paste("Test accuracy:", round(score$accuracy, 4)))

#d)
#Redefine data
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

#Reshape dimensions
x_train <- x_train %>% array_reshape(c(60000, 28, 28, 1))
x_test <- x_test %>% array_reshape(c(10000, 28, 28, 1))

#Scaling
x_train <- x_train / 255
x_test <- x_test / 255

#One-hot encoding; output as 0-9
y_train <- y_train %>% to_categorical(num_classes = 10)
y_test <- y_test %>% to_categorical(num_classes = 10)


model1 <- keras_model_sequential()

model1 %>% 
    #One convolutional layer
    layer_conv_2d(filters = 128, 
                  kernel_size = c(3, 3), 
                  activation = 'relu', 
                  input_shape = c(28, 28, 1)) %>% 
    layer_flatten() %>% 
    layer_dense(10, activation = 'softmax')

model1 %>% compile(optimizer = 'adam', 
                   loss = 'categorical_crossentropy',
                   metrics = c('accuracy'))

model1 %>% fit(x_train, y_train, epochs = 5, batch_size = 128)


score1 <- model1 %>% evaluate(x_test, y_test)

print(paste("Test accuracy:", round(score1$accuracy, 4)))



