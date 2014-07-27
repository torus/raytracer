# Heroku で Gauche<br/><small><small>（あるいは好きな言語なんでも）</small></small>

Lightweight Language Diver 2014

久井 亨 Toru Hisai @torus

.fx: titleslide

---

# Heroku

---

# Heroku

-   Y Combinator から生まれた PaaS
-   Git でスマートにデプロイ
    -   `git push heroku master`
-   アメリカ西海岸でブイブイいわせたい

<img src="http://upload.wikimedia.org/wikipedia/commons/1/16/Y-combinator-logo.gif"/>

---

# 様々な言語に対応
(スクリーンショット)

---

# しかし
-   Gauche はない
-   他の Scheme 処理系もない

---

# Heroku の構造

<!--
- アプリケーション
- Cedar Stack
- Dyno
- Heroku
-->

<img src="./Earth-crust-cutaway-english.svg" />

.fx: imageslide

---

# The Celadon Cedar Stack
-   Heroku の標準スタック
    -   ベース OS
    -   ライブラリ
    -   言語実行ランタイム
-   Ubuntu 10.04
-   自分でコンパイルすれば大抵使える
-   (図)

---

# Docker を使うと簡単
-   Docker → tar archive → ホストに転送
-   (図)

---

# Slug
-   実行可能な Cedar アプリケーションを tar アーカイブしたもの
-   約束はこれだけ：
    -   ディレクトリ名：app
    -   tar のコマンドライン：tar czfv slug.tgz ./app
        -   ./ が重要！

---

# デプロイ、リリース
-   Slug アーカイブをアップロードすると自動的にデプロイされる
    -   Ubuntu 10.04 でコンパイル
        -   glibc のバージョンなどが重要
    -   tar でアーカイブ
    -   Web API でアップロード
    -   Web API でリリース

---

# ところで
-   2014/7/20 Gauche 0.9.4 リリース！
-   (スクリーンショット)

---

# Gauche でウェブアプリ
-   Gauche の開発版
    -   プロダクション環境でも最新版を使いたい
    -   ビルドに Gauche のリリース版が必要
-   Gauche-makiki
    -   ウェブアプリケーションフレームワーク
-   Gauche-gl
    -   X11 関連のライブラリもビルド

---

# Dockerfile
-   Ubuntu 10.04
-   Gauche 0.9.4
-   Gauche HEAD

---

# Dyno
-   Dyno を増やすと簡単にスケールアウト
-   秒単位で料金精算
    -   $.05/時間

---

# 分散レイトレーシング
-   スクリーンを分割してレンダリング
-   ベクトル計算に Gauche-gl を使用

---

# レイトレーサの原理
-   (図)
-   視点から投影面の各ピクセルに対してレイ（光線）を飛ばす
-   反射、屈折
-   光源に達するまで再帰的に計算
