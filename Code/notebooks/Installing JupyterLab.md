## How to Install JupyterLab for Running a Local Notebook Server

### Prerequisites

-   You must have a valid installation of Python on your machine.

### Steps

1.  Install JupyterLab according to your appropriate Python installation. See [here](https://jupyterlab.readthedocs.io/en/latest/getting_started/installation.html).

2.  In an R console, run the following:

    1.  First:

        ``` r
        install.packages("devtools") 
        devtools::install_github("melff/RKernel/pkg")
        ```

    2.  Let the above code complete, then:

        ``` r
        RKernel::installspec()
        ```

3.  When installation of all packages is complete, run the following from a terminal located within your project root folder:

    ```         
    jupyter lab
    ```

    See [here](https://jupyterlab.readthedocs.io/en/latest/getting_started/starting.html) for more info.

<!-- -->

4.  If you were successful, you should see your browser open to the JupyterLab launcher, where you will see the R language as an installed Notebook and Console type.

    ![](../../Other%20Resources/JupyterLab.png)
