    title <- "在 rmarkdown 中一切变量皆可访问"

在 rmarkdown 中，文档的 metadata 默认是由 yaml 题头提供的。
如下面的样子。

    ---
    author: "gaospecial@gmail.com"
    output: md_document
    ---

但是这里面有一些高级的技巧。

yaml metadata 可以出现在任意位置
--------------------------------

<table>
<tbody>
<tr class="odd">
<td>title： “在文中也可以设置文档标题”</td>
</tr>
</tbody>
</table>

可以使用 R 中的变量
-------------------

yaml 配置也可以在 R chunk 中使用
--------------------------------

本文的作者是：

    rmarkdown::metadata$author

    ## [1] "gaospecial@gmail.com"

一切变量皆可访问，使得工作的灵活性大大增加。

例如：通过改变标题可以实现动态报告。

    for (state in state.name) {
      rmarkdown::render('input.Rmd', params = list(state = state))
    }

    ---
    title: "A report for `r params$state`"
    output: html_document
    ---

    The area of `r params$state` is
    `r state.area[state.name == params$state]`
    square miles.

参考资料
--------

-   Rmarkdown cookbook
    -   17.4：<https://bookdown.org/yihui/rmarkdown-cookbook/parameterized-reports.html#parameterized-reports>
    -   4.2：<https://bookdown.org/yihui/rmarkdown-cookbook/dynamic-yaml.html>
    -   4.3：<https://bookdown.org/yihui/rmarkdown-cookbook/document-metadata.html>
