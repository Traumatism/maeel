pub const MAEEL_STD_CONTENT: &str = r#"
λ println [] (print "\n" print ρ)
"#;

pub const MAEEL_STD_MATHS_CONTENT: &str = r#"
λ fact [] (δ 1 > ⇒ (δ↓ fact*))
λ sqrt [a] (a 2/→y 0 0 α ω (a y/ y+ 2/ →y↑ δ 5<) ρ ρ y)
λ log [n b] (0 n b ≥ ω (↑ n b / → n n b ≥))
λ pow [exp base] (base exp 0 ≠ ω (base * exp ↘ exp exp 0≠) base /)
λ gcd [a b] (b 0 ≠ ω (b ≕ _t a b % ≕ b _t ≕ a b 0 ≠) a)
"#;
