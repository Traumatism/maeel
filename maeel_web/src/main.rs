// Absolutly taken from tauri.app

use maeel_common::tokens::Token;
use maeel_interpreter::Interpreter;
use maeel_lexer::{extract_blocks, extract_instructions, lex_into_tokens};

use yew::prelude::*;

#[function_component(App)]
pub fn app() -> Html {
    let maeel_input_ref = use_node_ref();
    let mut interpreter = Interpreter::default();

    let name = use_state(String::new);
    let maeel_var_output = use_state(Vec::new);
    let maeel_stack_output = use_state(Vec::new);

    {
        let maeel_var_output = maeel_var_output.clone();
        let maeel_stack_output = maeel_stack_output.clone();
        let name = name.clone();
        let name2 = name.clone();

        use_effect_with_deps(
            move |_| {
                if name.is_empty() {
                    return;
                }

                let content = name.to_string();
                let tokens = lex_into_tokens(&content);

                if !tokens.iter().any(|e| matches!(e.token, Token::Include)) {
                    extract_instructions(extract_blocks(&lex_into_tokens(&content)))
                        .iter()
                        .for_each(|instruction| {
                            interpreter.handle_instruction(&mut instruction.iter())
                        });
                }

                maeel_var_output.set(
                    interpreter
                        .vars
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect(),
                );
                maeel_stack_output.set(interpreter.data.iter().map(|v| v.clone()).collect());
            },
            name2,
        );
    }

    let maeel = {
        let name = name;
        let maeel_input_ref = maeel_input_ref.clone();

        Callback::from(move |e: SubmitEvent| {
            e.prevent_default();
            name.set(
                maeel_input_ref
                    .cast::<web_sys::HtmlInputElement>()
                    .unwrap()
                    .value(),
            );
        })
    };

    let var_table = {
        let maeel_var_output = maeel_var_output.clone();
        html! {
            <table>
                <thead>
                    <tr><th>{"Name"}</th><th>{"Value"}</th></tr>
                </thead>
                <tbody>
                    {for maeel_var_output.iter().map(|(k, v)| {
                        html! {
                            <tr>
                                <td>{ k }</td>
                                <td>{ format!("{:?}", v) }</td>
                            </tr>
                        }
                    })}
                </tbody>
            </table>
        }
    };

    let stack_table = {
        let maeel_stack_output = maeel_stack_output.clone();
        html! {
            <table>
                <thead>
                    <tr><th>{"Value"}</th></tr>
                </thead>
                <tbody>
                    {for maeel_stack_output.iter().map(|v| {
                        html! {
                            <tr>
                                <td>{ format!("{:?}", v) }</td>
                            </tr>
                        }
                    })}
                </tbody>
            </table>
        }
    };

    html! {
        <main class="container">

            <form class="row" onsubmit={maeel}>
                <textarea id="maeel-input" ref={maeel_input_ref} placeholder="Enter an instruction" cols="60" rows="30" />
                <button type="submit">{"Execute"}</button>
            </form>

            <div>
                <h3>{"Vars:"}</h3>
                { var_table }
            </div>


            <div>
                <h3>{"Stack:"}</h3>
                { stack_table }
            </div>

        </main>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
