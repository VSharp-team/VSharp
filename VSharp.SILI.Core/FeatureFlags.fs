namespace VSharp.Core

module internal FeatureFlags =
    
    let mutable private _isConstraintIndependenceEnabled = false
    
    let public isConstraintIndependenceEnabled() = _isConstraintIndependenceEnabled
    
    let public setConstraintIndependenceEnabled enabled =
       _isConstraintIndependenceEnabled <- enabled
