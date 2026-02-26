fn main() {
    lalrpop::process_src().expect("failed to generate LALRPOP parsers");
}
