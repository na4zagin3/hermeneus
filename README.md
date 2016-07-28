# Hermeneus
A new translation framework that handles concordance.

See: <https://ja.wikipedia.org/wiki/%E4%B8%80%E8%87%B4>

## Project Structure
It consists three parts:
- Extractor
- Editor
- Renderer

- Handle concordance

## Use cases
Message translations are described with `i18n.trs`.
メッセージの翻訳は`i18n.rts`により行われる。
このメソッドは第一引数に翻訳キーを、引き続きプレースホルダーに埋められるべきものを引数として取る。
- `Integer`型や`Double`型などの数値はロカール依存の数値フォーマットによってフォーマットされる。 
  - 数の素性を持つ。
- `LocalizedWord`は翻訳文生成の際に素性の一致が考慮される。
  - `i18n.trw`() メソッドによって`LocalizedWord`を生成する。
  - `i18n.trw(singular, plural)` メソッドによって、デフォルト言語である英語において、数が単数であるときの語形と複数であるときの語形を指定した訳語を生成することができる。
- その他のオブジェクトは`String`型にキャストされそのまま埋め込まれる。
  - 素性を持たない。

### Shopping

#### ソースコード
```java
// com.example.diary.shopping
String buy(int number, itemId) {
  int number = 3;
   List<LocalizedWord> items = [ i18n.trw("car", "cars") // singular form: car, plural form: cars
                               , i18n.trw("book", "books")
                               , i18n.trw("pillar", "pillars")
                               ]
  String msg = i18n.trs("Bought {0} {1:number#0}.", number, items[itemId]).
}
```

#### 出力例
```
buy(0, 0) =>
  en-US: "Bought 0 cars."
  ja-JP: "0台の車を買った。"
buy(1, 0) =>
  en-US: "Bought 1 car."
  ja-JP: "1台の車を買った。"
buy(2, 0) =>
  en-US: "Bought 2 cars."
  ja-JP: "2台の車を買った。"
buy(2, 1) =>
  en-US: "Bought 2 books."
  ja-JP: "2冊の本を買った。"
```

#### 訳文データベース

##### ja
```yaml
properties:
  features:
    - name: "counter"
      values: dai, satsu, kire # 言語が持つ素性を列挙する。日本語の場合、名詞には序数詞を一致させる必要がある。

sentences:
  - sentence: "Bought {0:0~no,1~a} {1:数#0}."
    context: "com.example.shopping.buy"
    translation: "{0}{*助数詞:counter#1}の{1}を買った."

words:
  - word: "car"
    context: "com.example.shopping.buy"
    controller:
      counter: dai
    translation:
      - condition: "true"
        translation: "車"
  - word: "book"
    context: "com.example.shopping.buy"
    controller:
      counter: satsu
    translation:
      - condition: "true"
        translation: "本"
  - word: "助数詞" # 数値に関する
    context: "com.example.shopping.buy"
    agreement:
      counter: satsu
    translation:
      - condition:
          counter: dai
        translation: "台"
      - condition:
          counter: satsu
        translation: "冊"
```

##### en
```yaml
properties:
  features:
    - name: number
      values: # 数に関する条件の定義は https://www.gnu.org/software/gettext/manual/gettext.html#Translating-plural-forms を見ること
        - value: single
          condition "n=1"
        - value: plural
          condition "n!=1"

sentences:
  - sentence: "Bought {0:0~no,1~a} {1:数#0}."
    context: "com.example.shopping.buy"
    translation: "Bought {0:0~no,1~a} {1:数#0}."

words:
  - word: "car"
    context: "com.example.shopping.buy"
    controller: {}
    translation:
      - condition:
          number: single
        translation: "car"
      - condition:
          number: plural
        translation: "cars"
  - word: "book"
    context: "com.example.shopping.buy"
    controller: {}
    translation:
      - condition:
          number: single
        translation: "book"
      - condition:
          number: plural
        translation: "books"
```

### 複数の素性についての一致

```java
// com.example.diary.trip
List<String> countries = [ i18n.trw("Roma")
                         , i18n.trw("Athens")
                         ]
String msg1 = i18n.trs("Went to {0:city} from {1:city}.", list[0], list[1], means[0]).
String msg1 = i18n.trs("Went to {0:city} from {1:city}.", list[1], list[0], means[1]).
```

```
msg1 =>
  en-US: "Went to Roma from Athens ."
  ja-JP: "アテネからローマヘ行った。"
  grc-Latn: "εἰς ῾Ρώμην ἦλθον ἐξ Ἀθήνων."
msg2 =>
  en-US: "Went to Athens from Roma ."
  ja-JP: "ローマからアテネヘ行った。"
  grc-Latn: "εἰς Ἀθήνας ἦλθον ἐξ ῾Ρώμης." or "Ἀθήναζε ἦλθον ἐξ ῾Ρώμης." 
```

#### grcの翻訳データベース
```yaml
properties:
  features:
    - name: number
      values:
        - value: single
          condition "n=1"
        - value: plural
          condition "n=2"
        - value: dual
          condition "n!=1&&n!=2"
    - name: gender
      values:
        - value: mascurine
        - value: neuter
        - value: feminine
    - name: case
      values:
        - value: nominative
        - value: vocative
        - value: accusative
        - value: genetive
        - value: dative
    - name: position
      values:
        - value: final
        - value: nonfinal

sentences:
  - sentence: "Went to {0} from {1}."
    context: "com.example.trip"
    translation: "εἰς {0:case~accusative,position~nonfinal} ἦλθον ἐξ {1:case~genitive,position~final}."

words:
  - word: "Roma"
    context: "com.example.trip"
    controller:
      gender: feminine
      number: single
    translation:
      - condition:
          case: accusative
        translation: "῾Ρώμην"
      - condition:
          case: genitive
        translation: "῾Ρώμης."
  - word: "Athens"
    context: "com.example.trip"
    controller:
      gender: feminine
      number: plural
    translation:
      - condition:
          case: accusative
        translation: "Ἀθήνας"
      - condition:
          case: genitive
        translation: "Ἀθήνων."
```

