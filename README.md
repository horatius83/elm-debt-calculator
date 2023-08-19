# elm-debt-calculator
Program to calculate an optimal payment plan for numerous loans

# building
`.build.ps1`
or
`elm make .\src\Main.elm --optimize --output .\js\main.js`

# testing
`elm-test`

```mermaid
flowchart TD
    A(Find Bug)
    B[Create Branch]
    C[Write Test to Expose Bug]
    D{Fix Bug}
    E{Integration Tests}
    F[Merge Branch]
    G{Manual Tests QA}
    H[Deploy to Stage]
    I{Manual Tests Stage}
    J[Deploy to Prod]
    K{Manual Tests in Prod}
    L[Hotfix]

    A --> B --> C --> D
    D -->|Tests Pass| E
    D -->|Tests Fail| D
    E -->|Tests Pass| F
    E -->|Tests Fail| D
    F --> G
    G -->|Tests Pass| H
    G -->|Tests Fail| A
    H --> I
    I -->|Tests Pass| J
    I -->|Tests Fail| A
    J --> K
    K -->|Tests Fail| L
    L --> A
```
