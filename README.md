# Hermeneus
A new message-internationalization framework that handles agreement.

See: <https://en.wikipedia.org/wiki/Agreement_(linguistics)>

## Project Structure
This is (yet) a proof-of-concept implementation of the rendering engine of the new message i18n framework.

It will consists three parts:
- Extractor
- Editor
- Renderer

Stay tuned!

## Brief Introduction of the framework
A detailed paper may come.

Unlike [Gettext][gettext], message translation process in Hermeneus has two phases: _word translation_ and _sentence translation_. It also has _features_ (e.g., gender, starts with a consonant or a vowel [for distinction used by English _a-/an-_ and Hungarian _a-/az-_], and so on) other than numbers.

[gettext]: https://www.gnu.org/software/gettext/

A translated word can have _controllers_ and a _paradigm_ where a paradigm is a mapping from features to represent forms of words. For example translated word `*apple*` (hereinafter a translated is represented in monospaced font with surrounded `*`) in English controls `start-with-vowel` feature and has two representation forms: `singular`-number form “apple” and `plural`-number form “apples”.  And number `*10*` controls `plural`-number feature and has only representation form “10”.

Sentence translation is a process of constructing a translation result with appropriate representation forms of translation words.
In English, `*10* *apple*` will be translated into “10 apples” since `*10*` controls `plural`-number form of `*apple*`, which is “apples”.

This method properly works with other cases like _a-/an-_ distinction. Translated word `*a*` has two representation forms: “a” used before a word with `start-with-consonant` feature and “an” before a word with `start-with-vowel` feature.

##翻訳キーについて

```bnf
translation-string = { placeholder | general-string }
word-reference = { placeholder | '*' maybe-quoted-string }
placeholder = '{' word-reference [ ':' [ placeholder-attribute { ',' placeholder--attribute } ] ] '}'
word-reference = ( placeholder-number | word )
placeholder-attribute = feature-constraint | literal-pattern ; not yet implemented
literal-pattern = maybe-quoted-word [ '{' feature '=' maybe-quoted-string { ',' feature '=' maybe-quoted-string } '}' ] '~' maybe-quoted-word ; not yet implemented
feature-constraint = [ feature ] feature-expression
feature-expression = '#' word-reference
                   | '=' maybe-quoted-string
```

### Shopping

#### ソースコード
##### Haskell版
```haskell
wordCar  = ArgWord "car"  "cars" "com.example.shopping.buy"
wordBook = ArgWord "book" "books" "com.example.shopping.buy"
sentenceBought = MessageKey "Bought {0} {1:number#0}." "com.example.shopping.buy"

unit_translations :: IO ()
unit_translations = do
  db <- translationResource
  let msgBought l n w = translateMessage db l sentenceBought [ArgNumber n, w]
  msgBought "en" 1 wordCar @?= Right "Bought 1 car."
  msgBought "en" 2 wordCar @?= Right "Bought 2 cars." -- ToDo: Fix this
  msgBought "en" 2 wordBook @?= Right "Bought 2 books."
  msgBought "ja" 1 wordCar @?= Right "1台の車を買った。"
  msgBought "ja" 2 wordCar @?= Right "2台の車を買った。"
  msgBought "ja" 1 wordBook @?= Right "1冊の本を買った。"
```

##### Java版（予定）
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

出力例
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
    - name: "counter" # 助数詞
      values: dai, satsu, kire # 言語が持つ素性を列挙する。日本語の場合、名詞には序数詞を一致させる必要がある。

sentences: # sentence と context がキー
  - sentence: "Bought {0:0~no,1~a} {1:数#0}."
    context: "com.example.shopping.buy"
    translation: "{0}{*助数詞:#1}の{1}を買った."

words: # word と context がキー
  - word: "car"
    context: "com.example.shopping.buy"
    controller: # 持っている素性
      counter: dai
    translation:
      - condition: {}
        translation: "車"
  - word: "book"
    context: "com.example.shopping.buy"
    controller:
      counter: satsu
    translation:
      - condition: {}
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
          condition "n==1"
        - value: plural
          condition "n!=1"

sentences: # 
  - sentence: "Bought {0:0~no,1~a} {1:number#0}."
    context: "com.example.shopping.buy"
    translation: "Bought {0:0~no,1~a} {1:number#0}."

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

```haskell
wordRome   = ArgWord "Rome"   "" "com.example.diary.trip"
wordAthens = ArgWord "Athens" "" "com.example.diary.trip"
sentenceTrip = MessageKey "Went to {0} from {1}." "com.example.diary.trip"

unit_translations :: IO ()
unit_translations = do
  db <- translationResource
  translateMessage db "en"  sentenceTrip [wordRome, wordAthens] @?= Right "Went to Rome from Athens."
  translateMessage db "en"  sentenceTrip [wordAthens, wordRome] @?= Right "Went to Athens from Rome."
  translateMessage db "ja"  sentenceTrip [wordRome, wordAthens] @?= Right "アテネからローマへ行った。"
  translateMessage db "ja"  sentenceTrip [wordAthens, wordRome] @?= Right "ローマからアテネへ行った。"
  translateMessage db "grc" sentenceTrip [wordRome, wordAthens] @?= Right "εἰς Ῥώμην ἦλθον ἐξ Ἀθήνων."
  translateMessage db "grc" sentenceTrip [wordAthens, wordRome] @?= Right "εἰς Ἀθήνας ἦλθον ἐξ Ῥώμης."
```

Java版はこのようになる予定
```java
// com.example.diary.trip
LocalizedWord rome = i18n.trw("Rome");
LocalizedWord athens = i18n.trw("Athens");

String msg1 = i18n.trs("Went to {0} from {1}.", rome, athens).
String msg2 = i18n.trs("Went to {0} from {1}.", athens, rome).
```

```
msg1 =>
  en-US: "Went to Rome from Athens."
  ja-JP: "アテネからローマヘ行った。"
  grc-Latn: "εἰς Ῥώμην ἦλθον ἐξ Ἀθήνων."
msg2 =>
  en-US: "Went to Athens from Roma ."
  ja-JP: "ローマからアテネヘ行った。"
  grc-Latn: "εἰς Ἀθήνας ἦλθον ἐξ Ῥώμης." or "Ἀθήναζε ἦλθον ἐξ Ῥώμης."
```

#### grcの翻訳データベース

```yaml
properties:
  number:
    default: single
    values:
      - value: single
        condition: "n=1"
      - value: plural
        condition: "n=2"
      - value: dual
        condition: "n!=1&&n!=2"
  features: # ギリシャ語には素性が多い
    - name: gender # 数以外の素性には、その素性を持つための条件は存在しない。各語に対して指定される。
      values:
        - value: masculine
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
    context: "com.example.diary.trip"
    translation: "εἰς {0:case=accusative,position=nonfinal} ἦλθον ἐξ {1:case=genitive,position=final}."

words:
  - word: "Roma"
    context: "com.example.diary.trip"
    controller:
      gender: feminine
      number: single
    translation:
      - condition:
          case: accusative
        translation: "Ῥώμην"
      - condition:
          case: genitive
        translation: "Ῥώμης"
  - word: "Athens"
    context: "com.example.diary.trip"
    controller:
      gender: feminine
      number: plural
    translation:
      - condition:
          case: accusative
        translation: "Ἀθήνας"
      - condition:
          case: genitive
        translation: "Ἀθήνων"
```

