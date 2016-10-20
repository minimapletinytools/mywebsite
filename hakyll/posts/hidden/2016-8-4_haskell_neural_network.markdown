---
title: Haskell Neural Network
date: August 4, 2016
tags: blog, haskell, code, neural network
---

I've been wanting to play with a neural network for a while now. My classmate [Adam Ferriss](https://www.adamferriss.com/) made a bunch of pretty looking pictures while we were school together and of course there's that [Google thing](http://deepdreamgenerator.com/) that people went a little crazy over. So my goal was to implement a basic neural network and play around with it to generate images. Most such examples online use [Convolutional neural networks](https://en.wikipedia.org/wiki/Convolutional_neural_network) which are topologically inspired by the animal visual cortex and excel at image processing but I'm just doing a vanilla feed forward network here :). I followed [this introductory book](http://neuralnetworksanddeeplearning.com/) which is probably what you would find if you did any search for a neural network tutorial online. I quite reccomend it.



## Other links

This [hackage project](https://hackage.haskell.org/package/hnn) implements a weights only neural network using hmatrix and written to be "reasonably fast". I don't know why they did not include biases as they are trivial to implement and make some problems much easier to solve. It also comes with a complex (numbers) version as well.

[This](https://crypto.stanford.edu/~blynn/haskell/brain.html) project is similar to mine and also used [Neural Networks and Deep Learning](http://neuralnetworksanddeeplearning.com/) as a primary reference. It's implemented with the ReLu activation function (max(0,x)) and online training. Curiously enough, this project has about the same training rate and success rate as my network.

I found [this](https://github.com/jbarrow/LambdaNet) Haskell project on github providing an api for quickly testing different neural network algorithms. I thought this project was really cool though I'm happy tinkering with my code for the time being.