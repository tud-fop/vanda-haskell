

data Item nt t wt = Initial (Rule nt t, wt)   -- ^ grammar rule and weight
                            wt                -- ^ inside weight
                            wt                -- ^ outside weight
                  | Passive nt                -- ^ lhs's nonterminal
                            Ragevector        -- ^ spanned subword
                            (Derivation nt t) -- ^ grammar derivation
                            wt                -- ^ inside weight == weight of derivation
                  
newtype Container nt t wt = ( Map.HashMap nt [Item nt t]  -- ^ passive items, maps a to all passive with a on lhs
                            , Map.HashMap nt [Item nt t]  -- ^ active items, maps a to all active items with a in as
                            , Map.HashMap nt wt           -- ^ inside weights for each nonterminal
                            )
                  
initialPredicitons :: (Rule nt t, wt) -> DeductiveRule (Item nt t wt) wt a
initialPredicitons r@(Rule ((s, as), f), w) = DeductiveRule 0 gets app
  where
    gets :: a -> Item nt t -> [[Item nt t]]
    gets _ _ = [[]]
    
    app :: a -> [Item nt t] -> [Item nt t, wt]
    app (_, insides) [] = [(Initial r inside outside, inside <> outside)]
      where
        inside = w <> mconcat (map (insides Map.!) as)
        outside = mempty
    
prediction :: (Rule nt t, wt) -> DeductiveRule (Item nt t wt) wt a
prediction r@(Rule ((a, as), f), w) = DeductiveRule 1 gets app
  where
    gets :: a -> Item nt t -> [[Item nt t]]
    gets _ i@(Initial _ _ _) = [[i]]
    
    app :: a -> [Item nt t] -> [Item nt t]
    app (_, insides) [Initial (Rule ((_, as'), _)) inside' outside']
      | a `elem` as' = [(Initial r inside outside, inside <> outside)]
      where
        inside = w <> mconcat (map (insides Map.!) as)
        outside = inside' <-> (insides Map.! a) <> outside'
      | otherwise = []
      
completion :: DeductiveRule (Item nt t wt) wt a
completion = DeductiveRule (length as + 1) gets app
  where
    gets :: a -> Item nt t -> [[Itemt nt t]]
    gets (passives, _, _) i@(Initial (Rule ((_, as), _), _) inside' outside') = [ i:candidates
                                                                                | candidates <- mapM (passives Map.!) as
                                                                                ]
    gets (passives, actives, _) i@(Passive a _ _) = [ active:candidates
                                                    | active@(Initial (Rule ((_, as), _), _) _ _) <- Map.lookupDefault [] a actives
                                                    , candidates <- filter (any (== i)) $ mapM (passives Map.!) as
                                                    ]
                                                    
    app :: a -> [Item nt t] -> [Item nt t]
    app _ (Initial (r@(Rule ((a, _), f)) , w) _ outside : pas) =  [ (Passive rv $ node r ds, inside <> outside)
                                                                  | rv <- mapMaybe (insert rvs) $ instantiate w f
                                                                  ]
      where
        inside = mconcat ws <> w
        (rvs, ds, ws) = ...