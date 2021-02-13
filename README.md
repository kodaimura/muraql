# Muraql
Racket で GraphQL サーバを実装するためのライブラリ.\
GraphQL 仕様 http://spec.graphql.org に沿って開発しているが,\
いくつか未実装で対応していない機能もある.

### 対応
* query
* mutation
* Scalar型, Enum型, Union型, Object型, Input Object型, List型, Non-Null型
* Schema Introspection
* Fragment, Inline Fragment
* Validation

### 非対応
* subscription
* Variable
* Directive
* Interface型
* Schema Description

## 特徴
以下のリクエストで, スキーマをダウンロードできるようにした.
```
GET /graphql/schema
```


# インストール
以下のコマンドでインストール.

```
git clone https://github.com/kodaimura/muraql.git
```

# 使い方
Schema, Resolver を定義するだけで簡単に GraphQL サーバを起動できる. \
必要な関数は３つだけ 
* (define/schema)
* (define/resolver)
* (run-graphql)

基本的な使い方は以下の通り.

```scheme
#lang racket
(require "muraql/graphql.rkt")

(define/schema "type Query { hello : String }")

(define/resolver Query.hello
  "hello")

(run-graphql 4000)  
```

## define/schema 
GraphQL サーバで利用する型を定義する.\
型定義は, SDL(Schema Definition Language)で行う.

```scheme
(define/schema "

  type User {
    id : ID!
    name : String!
    age : Int
  }

  type Query {
    user (id : ID!) : User
  }

  schema {
  	query : Query
  }

")
```

型定義は別ファイルに記述し, file->string を利用する方法もある. 

```scheme
(define/schema (file->string "schema.graphql")) 
```

## define/resolver
* define/schema で定義した型のそれぞれのフィールドに対応する resolver を定義する.
* resolver はデータを返す関数, またはデータそのもの.

```scheme
(define/resolver Type.field 
  resolver)

;;省略型
(d/r Type.field
  resolver)
```

リクエストが来た際, 指定された field に対応する resolver が呼び出されることでレスポンスデータが組み立てられる.


### define/resolver 実装例1

```scheme
(define/schema "

  type Query {
  	hello : String!
  	numberOne : Int!
  	numberTwo : Int!
  	boolTrue : Boolean! 
  }

")


(define/resolver Query.hello 
  "hello")
(define/resolver Query.numberOne 
  1)
(define/resolver Query.numberTwo 
  2)
(define/resolver Query.boolTrue 
  #t)
```

これは, resolver をデータそのものとして与えているシンプルな例であり,\
以下のようなクエリを処理することができる.

```
query {
  hello
  numberTwo
}
```

### define/resolver 実装例2

次に, id でユーザを検索するような実装を考える.
つまり以下のようなクエリを処理できるようにする.

```
query {
  user (id : 1) {
  	id
  	name
  }
}
```

まず users データを用意.

```scheme
(define users
  (list #hash((id . 1) (name . "suzuki"))
        #hash((id . 2) (name . "matsui"))
        #hash((id . 3) (name . "ohtani"))))
```

define/scheme, define/resolver は以下のように実装する.

```scheme
(define/schema "

  type User {
  	id : Int!
  	name : String 
  }

  type Query {
  	user(id : Int!) : User
  }

")

(define/resolver Query.user
  (lambda (parent args info)
    (define id (hash-ref args 'id))
    (findf (lambda (u) (eq? id (hash-ref u 'id)))
           users)))

(define/resolver User.id
  (lambda (parent args info)
    (hash-ref parent 'id)))

(define/resolver User.name
  (lambda (parent args info)
     (hash-ref parent 'name)))

```

* resolver が関数の場合, 引数を3つとる関数にする.
* 引数は順番に parent, args, info
* parent ... 親階層のデータ
* args ... フィールドの引数
* info ... その他. schema情報等

```scheme
(lambda (parent args info)
  .....)
```

#### <参考> resolver が呼び出される流れ

```
query {
  user (id : 1) {
    id
    name
  }
}
```
上のクエリの場合
* Query.user の resolver が呼び出される. 
この際, args として #hash((id . 1)) が渡される \
その結果を parent として 
* User.id の resolver 
* User.name の rsolver が呼び出される. 


## デフォルト resolver
resolver を定義していないフィールドには, そのフィールド名をキーとするデータを抽出するデフォルト resolver が適用される.

parent データにそのフィールド名をキーとするデータがあれば, resolver 定義を省略できる. 

つまり, define/resolver 実装例2のこの部分は省略できる.

```scheme
;; parentデータ : #hash((id . 1) (name . "suzuki"))

(define/resolver User.id
  (lambda (parent args info)
    (hash-ref parent 'id)))

(define/resolver User.name
  (lambda (parent args info)
     (hash-ref parent 'name)))
```

## run-graphql
* GraphQL サーバ起動.
* /graphql でリクエストを受け付ける.
* 引数 port でポート番号を指定可能. デフォルト値は4000.

```scheme
(run-graphql [port]) 
```

# 実装例 (全体)
* コピー&ペーストでそのまま動く.

```scheme
#lang racket
(require "muraql/graphql.rkt")

(define playerdata
  (list #hash((id . 1) (name . "suzuki") (tid . 1))
        #hash((id . 2) (name . "matsui") (tid . 2))
        #hash((id . 3) (name . "ohtani") (tid . 3))))

(define teamdata
  (list #hash((id . 1) (name . "MARINERS"))
        #hash((id . 2) (name . "YANKEES"))
        #hash((id . 3) (name . "ANGELS"))))


(define/schema "

type Player {
  id : Int!
  name : String!
  team : Team
}

type Team {
  id : Int!
  name : String
}

type Query {
  allPlayers : [Player!]! 
  player(id : Int!) : Player
}
")


(define/resolver Query.allPlayers
  playerdata)

(define/resolver Query.player
  (lambda (parent args info)  
    (define id (hash-ref args 'id))
    (findf (lambda (p) (eq? id (hash-ref p 'id)))
           playerdata)))
 
(define/resolver Player.team
  (lambda (parent args info)
    (define tid (hash-ref parent 'tid))
    (findf (lambda (t) (eq? tid (hash-ref t 'id)))
           teamdata)))


(run-graphql 4000)  
```

## リクエスト方法例
* curl コマンド

```
curl "localhost:4000/graphql?query=\{player(id : 1) \{id name\}\}"
```

```
curl -X POST \
     -H "Content-Type: application/json" \
     -d '{"query" : "{player(id : 1) {id name}}"}' \
     http://localhost:4000/graphql
```

* Javacript Fetch

```javascript
let url = "http://localhost:4000/graphql"

let ops = {
   method: 'POST',
   headers: { 'Content-Type' : 'application/json'},
   body: JSON.stringify({ "query" : "{player(id : 1) {id name}}"})
}

fetch(url, ops)
  .then(res => res.json())
  .then(console.log)
  .catch(console.error)
```

# 応用編

## db 利用
デフォルト resolver を適用させるためには, ハッシュ型のデータを扱う必要がある.\
そこで, データベースからハッシュ型でデータを取得する関数を用意した. 

(query-all) ... 条件に当てはまる全てのデータをハッシュ型で取得 \
(query-get) ... 一番はじめに条件に当てはまるデータをハッシュ型で取得 \
(query-run) ... select 以外の SQL 処理 


```scheme
#lang racket
(require db)

(define sql3 (sqlite3-connect #:database "players.db"))

> (query-all sql3 "select * from players")
'(#hash((id . 1) (name . "suzuki") (tid . 1))
  #hash((id . 2) (name . "matsui") (tid . 2))
  #hash((id . 3) (name . "ohtani") (tid . 3)))

> (query-all sql3 "select * from players where id = ~a" 1)
'(#hash((id . 1) (name . "suzuki") (tid . 1)))

> (query-get sql3 "select * from players where id = ~a" 1)
'#hash((id . 1) (name . "suzuki") (tid . 1))

> (query-run sql3 "insert into players (name, tid) values ('~a' , ~a)" "tanaka" 2) 

```

## dmac/spin との併用
* Racket の RESTful Web app ライブラリ dmac/spin と併用することも可能.
* dmac/spin https://github.com/dmac/spin 

(run-graphql) の代わりに \
(set-spin-graphql define-handler) を用いる.\
これにより "/graphql" エンドポイントが作成される.

```scheme
#lang racket
(require "muraql/graphql.rkt")
(require (planet dmac/spin))

(define/schema ... )
(define/resolver ... )

(set-spin-graphql define-handler) 
;; "/graphql" エンドポイント作成

(run)  ;;dmac/spin起動
```

