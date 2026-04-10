use crate::model::language_model::Symbol;

pub fn format_class(i: &Symbol) -> String {
    if let Symbol::Class {
        name,
        variables,
        constructor,
        functions,
        ..
    } = i
    {
        return format!(
            r#"```ignite
class {name} {{
	{}{}{}
}}
```"#,
            if variables.len() > 0 {
                format!(
                    "{}\n",
                    variables
                        .iter()
                        .map(|x| {
                            if let Symbol::Variable {
                                name,
                                optional_type,
                                ..
                            } = x
                            {
                                if let Some(t) = optional_type {
                                    format!("let {name}: {t}")
                                } else {
                                    format!("let {name}")
                                }
                            } else {
                                unreachable!()
                            }
                        })
                        .collect::<Vec<_>>()
                        .join("\n\t")
                )
            } else {
                "".to_string()
            },
            if let Some(Symbol::Constructor { args, .. }) = constructor.as_deref() {
                format!("\n\tconstructor({})\n", args.join(", "))
            } else {
                "".to_string()
            },
            functions
                .iter()
                .map(|x| {
                    if let Symbol::Function { name, args, .. } = x {
                        format!("\n\tfn {name}({})", args.join(", "))
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>()
                .join(""),
        );
    }

    String::new()
}
