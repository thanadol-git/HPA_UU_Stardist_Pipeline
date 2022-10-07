# Read this
https://github.com/mrdbourke/m1-machine-learning-test
https://github.com/stardist/stardist

# Commands 

Before starting this code, we must set an environment for severla packages to be able to run with 
packages in Python. There are several important packages that need in order to run Stardist. One 
of them is Tensorflow. I have summarised waht the stardist persons concluded and the perfect ways 
to set up tensorflow here. Please check on the READ this for details. 

```
mkdir tensorflow-test
cd tensorflow-test
conda create --prefix ./env python=3.9
conda activate ./env
conda install -c apple tensorflow-deps
python -m pip install tensorflow-macos tensorflow-metal tensorflow-datasets      

pip install jupyterlab pandas numpy sklearn matplotlib stardist pyvips
#brew install vips

jupyter lab
```
