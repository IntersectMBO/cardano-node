# ドキュメンテーションを構築する

## 準備

```bash
brew install sphinx-doc
```

インストール後の手順がすべて実行されていることを確認します

```bash
sphinx-build -b html . builddir
pip3 install sphinx-rtd-theme
pip3 install recommonmark
pip3 install sphinx_markdown_tables --user
pip3 install sphinxemoji --user
```

## ドキュメンテーションを構築する

```bash
sphinx-build doc html
```

`html/index.html`でドキュメンテーションを開きます
