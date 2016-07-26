namespace VSharp.Core.Horn

type HornFacade(ctx : Microsoft.Z3.Context, fp : Microsoft.Z3.Fixedpoint, symbols : VSharp.Core.Common.SmtDeclarations) =
    let highlighterInstance = new HornHighlighter(ctx, fp)
    member public this.ctx = ctx
    member public this.fp = fp
    member public this.symbols = symbols
    member public this.highlighter = highlighterInstance
